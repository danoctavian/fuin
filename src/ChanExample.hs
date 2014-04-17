{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module ChanExample where

import Control.Concurrent.Chan
import Data.Char
import Control.Concurrent
import Control.Exception.Lifted as CEL
import Data.Typeable.Internal
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Base
import Control.Monad.Trans.Control

import Prelude as P

data MyException = ThisException | ThatException
     deriving (Show, Typeable)

instance Exception MyException

readWords :: Chan String ->  IO ()
readWords chan = do
  line <- getLine
  writeChan chan line
  readWords chan


printWords :: Chan String -> IO ()
printWords chan =  do
  line <- readChan chan
  putStrLn $ map toUpper line
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