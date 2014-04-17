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
import Data.Attoparsec 
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



type OutgoingPipe = (TChan ByteString, TChan ByteString)
type IncomingPipe = TChan ByteString
type TorrentFile = String

data Message = Data ByteString | SwitchChannel TorrentFile
  deriving (Eq, Show)


instance Serialize PackageStream.Message where
  put (Data bs) = p8 0 *> putByteString bs
  put (SwitchChannel torrentFile) = p8 1  *> putByteString (BSC.pack torrentFile)
  get =  getData <|> getSwitch

getData = byte 0 *> (Data <$> (remaining >>= getByteString))
getSwitch = byte 1 *> (SwitchChannel . BSC.unpack  <$> (remaining >>= getByteString))


streamOutgoing :: (MonadIO m) =>
    TChan PackageStream.Message -> OutgoingPipe -> EncryptF -> m ()
streamOutgoing appMessages pipe encrypt = do
  tryChanSource appMessages =$ CL.map (fmap (streamFormat . DS.encode))  $$ (outgoingSink pipe encrypt)
  return ()


outgoingSink :: (MonadIO m) => OutgoingPipe -> EncryptF -> Sink (Maybe ByteString) m Int
outgoingSink (input, output) encrypt = do
  packet <- liftIO $ atomically $ readTChan input
  bytes <- fmap BS.concat $ isolateWhileSmth (BS.length packet) =$ CL.consume
  liftIO $ atomically $ writeTChan output $ encrypt $ insertPayload packet bytes
  outgoingSink (input, output) encrypt


streamIncoming :: (MonadIO m, MonadThrow m) =>
    TChan PackageStream.Message -> IncomingPipe -> EncryptF -> m ()
streamIncoming appMessages pipe decrypt = do
  (chanSource pipe) $= (CL.map decrypt) $= (conduitParser parseMessage) $= (CL.map snd) $$ (chanSink appMessages)
  return ()
  


insertPayload :: ByteString -> ByteString -> ByteString
insertPayload oldPacket payload
  | lenPayload == 0 = oldPacket
  | lenPayload == lenOldPacket = payload
  | lenPayload < lenOldPacket = BS.concat [payload, padding (lenOldPacket - lenPayload)]
  | otherwise = error "len payload < len oldpacket"
    where
      lenPayload = BS.length payload
      lenOldPacket = BS.length oldPacket

streamFormat bs = BS.concat [messageHeader, DS.encode $ BS.length bs,
                            dataHeader, bs]

noDataMessage = streamFormat $ (BSC.pack "")

-- TODO: fix this flaw:
-- padding will have a minimum size of length noDataMessage 
-- How to: push back into the channel the unconsumed bytes of the padding they will be picked up
padding size = streamFormat $ BS.replicate (size - (BS.length noDataMessage)) (1 :: Word8)
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
parseMessage :: Parser PackageStream.Message
parseMessage = do
  string messageHeader
  n <- DAC.decimal
  string dataHeader
  body <- DAC.take n
  return $ PackageStream.Data body


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


parseSink :: (MonadIO m) => Sink (PositionRange, PackageStream.Message) m ByteString
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

