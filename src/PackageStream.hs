{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}

{- conduit pipeline

  SEND PIPE
  message chan => turn it into bytes => read byte arrays of N size (when a package comes in) 
  
  RECEIVE PIPE
  conn socket makes packages => turn into stream of bytes => parse packets as you get enough bytes => message chan
-}

module PackageStream where

import Utils
import Control.Monad
import Prelude as P
import Data.ByteString as BS
import Data.ByteString.Char8 as BSC
--import Data.ByteString.Lazy as BSL
import Data.Conduit as DC
import "conduit-extra" Data.Conduit.Binary as DCB
import Data.Conduit.Attoparsec
import qualified Data.Conduit.List as CL
import Control.Monad.IO.Class
import Control.Concurrent as Conc
import Data.Maybe
import Control.Exception (assert, finally)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Control.Monad.Catch
import Data.Attoparsec as DA
import Data.Attoparsec.Char8 as DAC
import Data.Binary
import System.Timeout 

import Data.Serialize as DS
import Data.Serialize.Put
import Data.Serialize.Get
import Control.Applicative hiding (empty)
import Control.Monad
import Data.Monoid

import Data.Either

import System.Log.Logger
import System.Log.Handler.Syslog
import System.Log.Handler.Simple

import BittorrentParser as BP
import Socks5Proxy
import Encryption


logger = "fuin.packageStream"

messageHeader :: ByteString
messageHeader = "MSG"
dataHeader :: ByteString
dataHeader = "DATA"


type Send = (MonadIO m) => PackageStream.Message -> m ()
type Receive = (MonadIO m) => m PackageStream.Message

data Packet = Packet ByteString | Kill
  deriving (Show, Eq)

type OutgoingPipe = (TChan Packet, TChan ByteString)
type IncomingPipe = TChan ByteString
type TorrentFile = String

data Message = Data ByteString | SwitchChannel TorrentFile | ClientGreeting ByteString
  deriving (Eq, Show)


sendMessage chan m = liftIO $ atomically $ writeTChan chan m
receiveMessage chan = liftIO $ atomically $ readTChan chan

instance Serialize PackageStream.Message where
  put (Data bs) = p8 0 *> putByteString bs
  put (SwitchChannel torrentFile) = p8 1  *> putByteString (BSC.pack torrentFile)
  put (ClientGreeting bs) = p8 2 *> putByteString bs
  get =  getData <|> getSwitch <|> getGreeting

getData = byte 0 *> (Data <$> (remaining >>= getByteString))
getSwitch = byte 1 *> (SwitchChannel . BSC.unpack  <$> (remaining >>= getByteString))
getGreeting = byte 2 *> (ClientGreeting <$> (remaining >>= getByteString))


-- collect the packet in the given chan
collectPacket chan bs = do 
  liftIO $ atomically $ writeTChan chan bs
  return bs 

-- send packet through a chan and get it through the other
transformPacket (inChan, outChan) bs = do
  liftIO $ atomically $ writeTChan inChan (Packet bs)
  liftIO $ atomically $ readTChan outChan


streamOutgoing :: (MonadIO m) =>
    TChan PackageStream.Message -> OutgoingPipe -> Encryption -> m ()
streamOutgoing appMessages pipe encryption = do
  tryChanSource appMessages =$ CL.map (fmap (streamFormat . DS.encode)) $$ (outgoingSink pipe encryption)
  return ()


outgoingSink :: (MonadIO m) => OutgoingPipe -> Encryption -> Sink (Maybe ByteString) m Int
outgoingSink (input, output) encryption = do
  chanPacket <- liftIO $ atomically $ readTChan input
  case chanPacket of
    (Packet packet) -> do
      bytes <- fmap BS.concat $ isolateWhileSmth (BS.length packet - (overhead encryption)) =$ CL.consume
      let (encryptedPayload, newEncryption) = (applyEncryption encryption) $ insertPayload packet bytes (overhead encryption)
      liftIO $ atomically $ writeTChan output $ encryptedPayload
      outgoingSink (input, output) newEncryption
    Kill -> return 0



streamIncoming :: (MonadIO m, MonadThrow m) =>
    TChan PackageStream.Message -> IncomingPipe -> Decryption -> m ()
streamIncoming appMessages pipe decryption = do
  liftIO $ debugM PackageStream.logger "streaming incoming traffic"
  incomingPackageSource pipe decryption $$ (chanSink appMessages)
  return ()

incomingPackageSource pipe decryption
  = (chanSource pipe) $= (CL.map (decrypt decryption)) $= CL.filter (/= Nothing) $= CL.map fromJust
        $= CL.mapM (\m -> (liftIO $ debugM PackageStream.logger $ "message incoming " ++ show m) >> return m)
        $= (conduitParser (DA.skipWhile (pad == ) >> parseMessage)) $= (CL.map snd) $=
        (CL.filter isRight) $= (CL.map fromRight)


-- kind of hacky but what to do...
noOpStreamOutgoing :: (MonadIO m) => OutgoingPipe -> m ()
noOpStreamOutgoing p@(input, output) = do
  packet <- liftIO $ atomically $ readTChan input
  case packet of 
    Packet bs -> do
      liftIO $ atomically $ writeTChan output $ bs
      noOpStreamOutgoing p
    Kill -> return () --be done with it

-- only do it on a real payload; leave things untouched if there is nothing to send
applyEncryption :: Encryption -> Either ByteString ByteString  -> (ByteString, Encryption)
applyEncryption e (Left oldPacket) = (oldPacket, e)
applyEncryption e (Right payload) = encrypt e payload 


-- return Left s with the package unchanged if no payload exists1
insertPayload :: ByteString -> ByteString -> Int -> Either ByteString ByteString
insertPayload oldPacket payload encrOverhead
  | lenPayload == 0 = Left oldPacket
  | lenPayload +encrOverhead == lenOldPacket = Right payload
  | lenPayload + encrOverhead < lenOldPacket = Right $ BS.concat [payload, padding (lenOldPacket - lenPayload)]
  | otherwise = error "len payload < len oldpacket"
    where
      lenPayload = BS.length payload
      lenOldPacket = BS.length oldPacket

streamFormat bs = BS.concat [messageHeader, DS.encode $ BS.length bs,
                            dataHeader, bs]

noDataMessage = streamFormat $ (BSC.pack "")


-- padding value
pad :: Word8
pad = 69
-- TODO: fix this flaw:
-- padding will have a minimum size of length noDataMessage 
-- How to: push back into the channel the unconsumed bytes of the padding they will be picked up
-- padding contains bytes of data that are meant to fail when attempted to be deserialized -hence the 69
padding size = BS.replicate size pad
{-
data Message = Message ByteString
  deriving Show
-}
-- isolate n bytes OR until Nothing is encountered
isolateWhileSmth :: Monad m
        => Int
        -> Conduit (Maybe BS.ByteString) m BS.ByteString
isolateWhileSmth =
    loop
  where
    loop 0 = return ()
    loop count = do
        mbs <- await
        case mbs of
            Nothing -> return ()
            Just Nothing -> return ()
            Just (Just bs) -> do
                let (a, b) = BS.splitAt count bs
                case count - BS.length a of
                    0 -> do
                        unless (BS.null b) $ leftover $ Just b
                        DC.yield a
                    count' -> assert (BS.null b) $ DC.yield a >> loop count'



tryChanSource :: (MonadIO mio) => TChan a -> Source mio (Maybe a)
tryChanSource chan = do
  res <- liftIO $ atomically $ tryReadTChan chan
  DC.yield res
  tryChanSource chan

chanSource :: (MonadIO m) => TChan a -> Source m a
chanSource chan = do
  res <- liftIO $ atomically $ readTChan chan
  DC.yield res
  chanSource chan


chanSink :: (MonadIO m) => TChan a -> Sink a m ()
chanSink chan = do
  x <- await
  when (isJust x) $ liftIO $ atomically $ writeTChan chan (fromJust x)
  chanSink chan
 
getBTPacket :: BS.ByteString -> Either String BP.Message
getBTPacket = DS.decode


isPiece (Piece _ _ _) = True
isPiece _ = False

-- unwrap and wrap back a bittorrent piece after applying a transform
pieceHandler :: PacketHandler -> PacketHandler
pieceHandler trans bs = do
  case (getBTPacket bs) of
    Left err -> do
      liftIO $ errorM PackageStream.logger
        ("proxied package is not bittorrent. length: " ++ (show $ BS.length bs))
      return bs -- continue running
    Right packet -> if' (isPiece packet)
                ((\(Piece num size payload) -> trans payload
                    >>= (return . DS.encode . (Piece num size))) packet)
                (return bs) -- do nothing if it isn't a piece

-- to turn them into bytes just run CL.map serialize on the stream; preserving the maybe stuff


-- TODO: write message reading 
{-
  RECEIVE PIPE
  conn socket makes packages => turn into stream of bytes => parse packets as you get enough bytes => message chan

  SOURCE: read message from socket; push the bytes; parse stream of messages; SINK - Tchan for messages
  -}

-- message between server and client
parseMessage :: Parser (Either String PackageStream.Message)
parseMessage = do
  string messageHeader
  n <- fmap (\i -> (fromRight $ DS.decode i) :: Int) $ DAC.take 8
  string dataHeader
  body <- DAC.take n
  return $ DS.decode body


dataFromPacket :: Packet -> ByteString
dataFromPacket (Packet bs) = bs

-- toy crap from here onwards 

byteSource :: Source IO ByteString
byteSource = do
  DC.yield "xxx"
  --liftIO $ Conc.threadDelay $ 10 * 10 ^ 6
  DC.yield "ZZZZZYZZ"
  return ()
  --byteSource



maybeByteSource :: Source IO (Maybe ByteString)
maybeByteSource = do
  DC.yield $ Just "XXXXX"
  DC.yield $ Nothing
  DC.yield $ Just "YYYYYYYY"
  DC.yield $ Nothing


parseByteSource :: Source IO ByteString
parseByteSource = do
  let (bs1, bs2) = BS.splitAt 5  $ byteMessage 10
  DC.yield $ bs1
  DC.yield $ bs2
  --DC.yield $ 
  DC.yield $ byteMessage 4

byteMessage size = BS.concat [messageHeader, BS.pack $ strToWord8s $ show size, dataHeader, BS.replicate size (1 :: Word8)]

maybeByteSink :: Sink (Maybe ByteString) IO ByteString
maybeByteSink = do
  liftIO $ P.putStrLn "give input"
  n <- fmap (read :: String -> Int) $ liftIO $ P.getLine
  bytes <- isolateWhileSmth n =$ CL.consume
  liftIO $ P.putStrLn $ "read bytes are " ++ (show bytes)
  maybeByteSink


bytesink :: Sink ByteString IO ByteString
bytesink = do
  liftIO $ P.putStrLn "give input"
  n <- fmap (read :: String -> Int) $ liftIO $ P.getLine
  --bytes <- DCB.take n
  let delim = strToWord8s "Y" !! 0
  bytes <- DCB.takeWhile (\x -> x /= delim) =$ (DCB.take n)
  head <- DCB.head
  when (isJust head && fromJust head /= delim) (leftover $ BS.pack [fromJust head]) 
  liftIO $ P.putStrLn $ "read bytes are " ++ (show bytes)

  bytesink


parseSink :: (MonadIO m) => Sink (PositionRange, Either String PackageStream.Message) m ByteString
parseSink = do
  msg <- await
  liftIO $ P.putStrLn $ show msg
  msg <- await
  liftIO $ P.putStrLn $ show msg
  return ""
  
--runStream :: IO ()
runStream = do
  liftIO $ P.putStrLn "running stream"
  res <- parseByteSource $= (CL.map id) $= (conduitParser parseMessage) $$ parseSink
  return ()

runMaybeStreamTest = do
  liftIO $ P.putStrLn "running maybe stream test"
  res <- maybeByteSource $$ maybeByteSink
  return ()


samplePackage01 = "\69\69MSG\NUL\NUL\NUL\NUL\NUL\NUL\NUL!DATA\STX\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOHMSG\NUL\NUL\NUL\NUL\NUL\NUL\NUL%DATA\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH"

testParseMsg = parse (DA.skipWhile (pad == ) >> parseMessage) samplePackage01
testSerialize :: Either String PackageStream.Message
testSerialize = DS.decode $ DS.encode $ ClientGreeting $ BSC.pack "matah"