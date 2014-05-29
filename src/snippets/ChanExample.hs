{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module ChanExample where

import Prelude as P
import Control.Concurrent.Chan
import Data.Char
import Control.Concurrent
import Control.Exception.Lifted as CEL
import Data.Typeable.Internal
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.ByteString.Char8 as DBC
import Data.Conduit.Binary as DCB
import Data.Char

import Data.Conduit as DC
import qualified Data.Conduit.List as CL

import Prelude as P

data MyException = ThisException | ThatException
     deriving (Show, Typeable)

instance Exception MyException

readWords :: Chan String ->  IO ()
readWords chan = do
  line <- P.getLine
  writeChan chan line
  readWords chan


printWords :: Chan String -> IO ()
printWords chan =  do
  line <- readChan chan
  P.putStrLn $ P.map toUpper line
  printWords chan


runChans = do
  c <- newChan
  forkIO $ printWords c
  readWords c

maybeThrow :: (MonadBase IO m, MonadIO m) =>  Int -> m Int
maybeThrow x = do
  when (x `mod` 2 == 1) $ CEL.throwIO ThisException
  return $ x `div` 2

runExceptionCode :: (MonadBase IO m, MonadBaseControl IO m, MonadIO m) => m ()
runExceptionCode = do
  x <- CEL.try (maybeThrow 1)
  case x of 
    Left (e :: MyException) -> return 1
    Right x ->  return 2 
  return ()
  --CEL.catch (maybeThrow 1)  ((\e -> return 1) :: MyException -> IO Int)

fooork :: (MonadIO m) => m ()
fooork = do
  liftIO $ P.putStrLn "foooork"

runFork :: (MonadIO m) => m ()
runFork = do
  liftIO $ P.putStrLn "running a fork"
  id <- liftIO $ forkIO $ liftIO fooork 
  return ()


byteSource =
  DC.yield (DBC.pack "hellomotherfucker")


prntSink = awaitForever (\x -> liftIO $ P.putStrLn $ show x)

takes = do
  bs <- DCB.takeWhile ((fromIntegral $ ord 'o') /=) =$ (DCB.take 10)
  DC.yield bs

testTakes = byteSource =$ takes $$ prntSink

resumables = do
    (rsrc1, result1) <- CL.sourceList [1..10] $$+ CL.take 3
    (rsrc2, result2) <- rsrc1 $$++ CL.take 3
    (rsrc3, result3) <- rsrc2 $$++ CL.take 2
    result4 <- rsrc3 $$+- CL.consume
    print (result1, result2, result3, result4) 