{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE  ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module FakeServer where

import Control.Concurrent
import Control.Monad.IO.Class
import System.Log.Logger
import System.Log.Handler.Syslog
import System.Log.Handler.Simple

import Server
import Encryption 

logger = "fuin.fakeServer"

-- echo server
handleConnection :: HandleConnection
handleConnection (send, receive) = do
  msg <- receive 
  liftIO $ debugM FakeServer.logger $ show msg
  send msg
