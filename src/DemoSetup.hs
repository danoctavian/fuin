{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE  ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module DemoSetup where


import Prelude as P
import Control.Monad
import Control.Monad.IO.Class
import Socks5Proxy
import Network
import Network.Socks5
import Data.ByteString as DBS
import Data.ByteString.Char8 as DBC
import UTorrentAPI
import Data.Word
import System.Log.Logger
import System.Log.Handler.Syslog
import System.Log.Handler.Simple
import System.IO
import System.Random
import Network.Socket
import Control.Monad.Error
import Control.Monad.Trans.Control
import Control.Concurrent.STM
import Control.Concurrent
import Data.Map as DM
import System.Timeout
import Network.Socks5
import GHC.IO.Handle as IOH
import Network.Socket.ByteString as NBS
import Network.Socket as NS

import TorrentClient
import Client
import Server
import PackageStream
import Socks5Proxy
import Encryption

import Data.Conduit
import Data.Either
import Data.Conduit.List as DCL
import Utils
import BittorrentParser
import Data.Serialize as DS
import TorrentFileParser
import UTorrentAPI

import qualified FakeClient as FC

logger = "fuin.demo"
-- SETUP for the demo

-- ASSSSUMPTIONS
-- torrent clients are assumed to be setup (with the right settings)

torrentFile  = ("/home/dan/test/bigFile.dan.torrent", "/home/dan/test/bigFile.dan")

receiverPublicPort = 6891
receiverPrivatePort = 8888
socks5ProxyPort = 1080

runDemoClient = do
  FC.setupLoggers 

  liftIO $ debugM DemoSetup.logger "running client.."
  connResult <- runErrorT $ do 
    transporter <- Client.init (PortNumber $ fromIntegral socks5ProxyPort)
                                $ makeUTorrentConn localhost 8080 ("admin", "")
    let info = ServerInfo {clientEncyption = makeClientEncryption fakeKey fakeKey fakeKey,
              serverTorrentFile  = (Right $ fst torrentFile, torrentFile),
              -- no change to the endianess of the port since this is sent over the network
              serverSockAddr = SockAddrInet (PortNum $ toggleEndianW16 receiverPublicPort) $ FC.localhostW32
              }
    Client.makeConn transporter info
  case connResult of
    Right (send, recv) -> do
      liftIO $ debugM DemoSetup.logger "the client is running"
      send $ Data $ DBC.pack "hello from client"
    Left errMsg ->
      liftIO $ errorM DemoSetup.logger $ "connection failed with " P.++ (show errMsg)
  return ()

runDemoServer = do
  FC.setupLoggers
  return ()
  liftIO $ debugM DemoSetup.logger "running server.."
  Server.run FC.echoHandleConn torrentFile (PortNumber receiverPublicPort) (PortNumber receiverPrivatePort)
             (makeServerBootstrapEncryption fakeKey fakeKey) 