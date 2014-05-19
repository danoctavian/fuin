{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE  ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module FakeClient where

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
import Data.Conduit.List as DCL
import Utils
import BittorrentParser
import Data.Serialize as DS

logger = "fuin.fakeClient"

localhostW32 = readIPv4 "127.0.0.1"

-- bytes
packetSize = 100

milli = 10 ^ 6
{-

FAKES
initiator connects on port 6881

receiver listens
on port 3333

-}

receiverPublicPort = 6668
receiverPrivatePort = 3337
socks5ProxyPort = 1080


runFakeBTClientInitiator pingChan = do
  liftIO $ debugM FakeClient.logger "running fake client initiator.."
  liftIO $ atomically $ readTChan pingChan
  liftIO $ debugM FakeClient.logger "starting connection.."
  sock <- socket AF_INET Stream defaultProtocol
  socksConnectAddr sock (SockAddrInet (PortNum $ toggleEndianW16 socks5ProxyPort) $ localhostW32)
                        (SockAddrInet (PortNum $ receiverPublicPort) $ localhostW32)
  liftIO $ debugM FakeClient.logger "connected to socks5 proxy"
  foreverSendReceive sock
  {-
    handle <- socksConnectTo localhost (PortNumber 1080) localhost (PortNumber 6669)
    IOH.hPutStr handle "wtfmom"
  -}

runFakeBTClientReceiver = do
  liftIO $ debugM FakeClient.logger "running fake client receiver.."
  serverSock <- listenOn (PortNumber receiverPrivatePort)
  (sock, sockAddr) <- NS.accept serverSock
  liftIO $ debugM FakeClient.logger "connection received!"
  foreverSendReceive sock
  return ()
 -- foreverSendReceive sock

packPayload = DBS.replicate packetSize (fromIntegral 2)

foreverSendReceive sock = do
  liftIO $ forkIO $ forever $ NS.recv sock (2 * packetSize) >> threadDelay (1 * milli) 
  forever $ (NBS.send sock $ DS.encode (Piece 69 0 (packPayload))) >> threadDelay (1 * milli) 

--runFakeBTClientReceiver = d

type PingChan = TChan Int
pingMsg = 23

makeFakeTorrentClientConn :: PingChan -> MakeTorrentClientConn
makeFakeTorrentClientConn pingChan = return $ TorrentClientConn {
                            addMagnetLink = (\s -> liftIO $ atomically $ writeTChan pingChan pingMsg >>  return () ),
                            listTorrents = return [],
                            pauseTorrent = (\h -> return () ),
                            setProxySettings = (\sets -> (liftIO $ debugM FakeClient.logger $ show sets) >>  return ())
                        }

runFuinClient = do
  setupLoggers
  pingChan <- liftIO $ atomically $ newTChan 
  liftIO $ forkIO $ runFakeBTClientInitiator pingChan
  connResult <- runErrorT $ do 
    transporter <- Client.init (PortNumber $ fromIntegral socks5ProxyPort) $ makeFakeTorrentClientConn pingChan
    let info = ServerInfo {clientEncyption = makeClientEncryption fakeKey fakeKey fakeKey,
              serverTorrentFile = "notimportant",
              -- no change to the endianess of the port since this is sent over the network
              serverSockAddr = SockAddrInet (PortNum $ toggleEndianW16 receiverPublicPort) $ localhostW32
              }
    Client.makeConn transporter info
  case connResult of
    Right (send, recv) -> do
      testCommunicationClient (send, recv)
      return ()
    Left errMsg ->
      liftIO $ errorM FakeClient.logger $ "connection failed with " P.++ (show errMsg)
  -- debugM FakeClient.logger $ "got message with " P.++  (show serverMsg)
  return ()

runFuinServer = do
  setupLoggers
  liftIO $ debugM FakeClient.logger "running server.."
  liftIO $ forkIO $ runFakeBTClientReceiver
  Server.run echoHandleConn (PortNumber receiverPublicPort) (PortNumber receiverPrivatePort)
             (makeServerBootstrapEncryption fakeKey fakeKey) 


setupLoggers
  = forM [FakeClient.logger, Socks5Proxy.logger, PackageStream.logger, Client.logger, Server.logger] 
      (\lg -> liftIO $ updateGlobalLogger lg (setLevel DEBUG))


testCommunicationClient ch@(send, recv) = do
  liftIO $ debugM FakeClient.logger "writing message to server "
  send $ Data $ DBC.pack "this is the client motherfucker"
  serverMsg <- recv
  liftIO $ debugM FakeClient.logger $ "received message from server  " P.++ (show serverMsg)
  liftIO $ threadDelay $ 10 ^ 6
  testCommunicationClient ch

echoHandleConn :: HandleConnection
echoHandleConn chans@(send, receive)
  = do
    msg <- receive
    liftIO $ debugM FakeClient.logger $ "Received message " P.++ (show msg)
    send msg
    (echoHandleConn chans)

