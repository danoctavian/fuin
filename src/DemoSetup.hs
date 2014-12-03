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
  setupLoggers 

  liftIO $ debugM DemoSetup.logger "running client.."
  connResult <- runErrorT $ do 
    transporter <- Client.init (PortNumber $ fromIntegral socks5ProxyPort)
                                $ makeUTorrentConn "localhost" 8080 ("admin", "")
    let info = ServerInfo {clientEncyption = makeClientEncryption fakeKey fakeKey fakeKey,
              serverTorrentFile  = (Right $ fst clientSideTorrentFile, clientSideTorrentFile),
              -- no change to the endianess of the port since this is sent over the network
              serverSockAddr = SockAddrInet (PortNum $ toggleEndianW16 receiverPublicPort) $ readIPv4 "129.31.191.89"
              }
    Client.makeConn transporter info
  case connResult of
    Right (send, recv) -> do
      foreverChat "client" (send, recv)
      {-
      liftIO $ debugM DemoSetup.logger "the client is running"
      send $ Data $ DBC.pack "hello from client"
      response <- recv
      liftIO $ debugM DemoSetup.logger $ "the response from the server is " ++ (show response)      
      liftIO $ threadDelay $ 10 ^ 9
      -}
    Left errMsg ->
      liftIO $ errorM DemoSetup.logger $ "connection failed with " P.++ (show errMsg)
  return ()

clientSideTorrentFile = ("/homes/dco210/demoFiles/bigFile.dan.torrent", "/homes/dco210/demoFiles/bigFile.dan")

runDemoServer = do
  setupLoggers
  return ()
  liftIO $ debugM DemoSetup.logger "running server.."
  Server.run (foreverChat "server") torrentFile (PortNumber receiverPublicPort) (PortNumber receiverPrivatePort)
             (makeServerBootstrapEncryption fakeKey fakeKey) 

setupLoggers
  = forM [DemoSetup.logger, Socks5Proxy.logger, PackageStream.logger, Client.logger, Server.logger, UTorrentAPI.logger] 
      (\lg -> liftIO $ updateGlobalLogger lg (setLevel DEBUG))

foreverChat :: String -> HandleConnection
foreverChat partner chans@(send, recv) = do
  liftIO $ debugM DemoSetup.logger $ "forever chatting as " ++ partner
  send $ Data $ DBC.pack $ "You are now chatting to " ++ partner
  --warmup 
  forM [1..100] $ \i -> send $ Data $ DBC.pack $ "warm up message number " ++ (show i) ++ " sent by " ++ partner
  liftIO $ forkIO $ forever (recv >>= (\msg -> liftIO $ debugM DemoSetup.logger $ "message received: " ++ (show msg)))
  forever $ do
    line <- liftIO $ P.getLine
    send $ Data $ DBC.pack line

echoHandleConn :: HandleConnection
echoHandleConn chans@(send, receive)
  = do
    msg <- receive
    liftIO $ debugM DemoSetup.logger $ "Received message " P.++ (show msg)
    liftIO $ debugM DemoSetup.logger $ "echoing back... "
    send msg
    (echoHandleConn chans)
