{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


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
import Data.Attoparsec 
import Data.Attoparsec.Char8 as DAC
import Data.Binary


messageHeader :: ByteString
messageHeader = "MSG"
dataHeader :: ByteString
dataHeader = "DATA"
data Message = Message ByteString
  deriving Show

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



-- to turn them into bytes just run CL.map serialize on the stream; preserving the maybe stuff


-- TODO: write message reading 
{-
  RECEIVE PIPE
  conn socket makes packages => turn into stream of bytes => parse packets as you get enough bytes => message chan

  SOURCE: read message from socket; push the bytes; parse stream of messages; SINK - Tchan for messages
  -}


parseMessage :: Parser Message
parseMessage = do
  string messageHeader
  n <- DAC.decimal
  string dataHeader
  body <- DAC.take n
  return $ Message body


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
  DC.yield $ byteMessage 3
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
  when (isJust head && fromJust head /= delim) (leftover $ pack [fromJust head]) 
  liftIO $ P.putStrLn $ "read bytes are " ++ (show bytes)

  bytesink


parseSink :: Sink (PositionRange, Message) IO ByteString
parseSink = do
  msg <- await
  liftIO $ P.putStrLn $ show msg
  msg <- await
  liftIO $ P.putStrLn $ show msg
  return ""
  
--runStream :: IO ()
runStream = do
  liftIO $ P.putStrLn "running stream"
  res <- parseByteSource =$ (conduitParser parseMessage) $$ parseSink
  return ()


