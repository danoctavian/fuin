{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE  ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Server where

import Prelude as P
import Utils
import Control.Monad
import Control.Monad.IO.Class
import Socks5Proxy
import Network
import Data.ByteString as DBS
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

import TorrentClient
import PackageStream
import Socks5Proxy
import Data.Conduit
import Data.Conduit.List as DCL

import Encryption

logger = "fuin.server"

type MonadFuinServer m = (MonadIO m)
type HandleConnection = (MonadFuinServer m) => (Send, Receive) -> m ()


-- TODO: figure out port/port-range for which to reverse proxy
-- need to know how bittorrent finds ot where to connect to a certain peer (DHT?)
run :: (MonadFuinServer m) => HandleConnection -> PortID -> PortID -> BootstrapServerEncryption -> m ()
run handleConnection publicPort internalPort bootstrapEnc = do
  liftIO $ debugM Server.logger "running fuin server..."
  runReverseProxy publicPort internalPort $
                    serverReverseProxyInit handleConnection bootstrapEnc -- magic ports; standard bittorrent port + magic bittorrent client port
  

runReverseProxy :: (MonadIO m) => PortID  -> PortID -> InitHook -> m ()
runReverseProxy listenPort targetPort initHook
  = runServer (Config  listenPort initHook
            (\s -> return $ Socks5Proxy.Connection CONNECT IPV4 $ SockAddrInet
                  (PortNum $ toggleEndianW16 ((\(PortNumber n) -> fromIntegral n :: Word16) targetPort))
                   $ readIPv4 "127.0.0.1"))

serverReverseProxyInit :: HandleConnection -> BootstrapServerEncryption -> InitHook
serverReverseProxyInit handleConnection bootstrapEnc clientSock serverSock = do
  liftIO $ debugM Server.logger "initializing reverse proxy connection..."
  [incomingPipe, outgoingOut] <- replicateM 2 (liftIO $ atomically newTChan)
  outgoingIn <- (liftIO $ atomically newTChan)
  let outgoingPipe = (outgoingIn, outgoingOut) :: (TChan Packet, TChan ByteString)
  -- TODO; receive packets until you get the bootstrap package or timeout is reached
  liftIO $ forkIO $ checkForClientConn incomingPipe outgoingPipe bootstrapEnc $
                    makeClientConn incomingPipe outgoingPipe handleConnection
  -- while just forwarding the sent packages...
  liftIO $ forkIO $ noOpStreamOutgoing outgoingPipe

  return $ PacketHandlers {
   -- incoming packets are packets from the Bittorrent reverse proxied server
    incoming = (pieceHandler $ transformPacket outgoingPipe),
    outgoing = (pieceHandler $ collectPacket incomingPipe)
    -- outgoing packets are packets send by the connection initiator, which is the client
  }


checkForClientConn incomingPipe (inputOutgoing, outputOutgoing) bootstrapEnc makeConnection = do
  liftIO $ debugM Server.logger "reading greeting message..."
  greeting <- liftIO $ timeout (10 ^ 7) $ (incomingPackageSource incomingPipe
                                      (bootstrapServerDecrypt bootstrapEnc)) $$ (DCL.take 1)
  liftIO $ debugM Server.logger "got something for a greeting"
  case greeting of
    Just [ClientGreeting bs] -> do
      liftIO $ debugM Server.logger "got a good result lads!" 
      liftIO $ atomically $ writeTChan inputOutgoing Kill -- kill the noOp thread
      makeConnection $ makeServerEncryption bootstrapEnc bs
    _ ->  liftIO $ do
        debugM Server.logger "it's not a client connection. run a normal proxy."
        -- TODO: clean this up... it's a waste of computation
        forever (liftIO $ atomically $ readTChan incomingPipe) -- just emtpying the chan...
  return ()


makeClientConn :: (MonadFuinServer m) => IncomingPipe -> OutgoingPipe ->
                  HandleConnection -> ServerEncryption -> m ()
makeClientConn incomingPipe outgoingPipe handleConnection encryption
  = do
    [sendChan, receiveChan] <- replicateM 2 (liftIO $ atomically newTChan)
    liftIO $ forkIO $ streamOutgoing sendChan outgoingPipe (serverEncrypt encryption)
    liftIO $ forkIO $ streamIncoming receiveChan incomingPipe (serverDecrypt encryption)

    sendMessage sendChan AckGreeting
    -- call the custom connection handler
    handleConnection ((sendMessage sendChan), (receiveMessage receiveChan))
    return ()


{-
init: start reverse proxy
<no torrent client settings atm>
give connection handler (some function to call when a connection is made)

in init - need to check that the connection is the type we wanted
  technique: attempt decryption; if no packets succeed after n packs make it a normal connection
  for this spawn a thread  to handle this crap


  seems simpler than the client
-}

{-
runFuinServer :: (MonadIO m) => m ()
runFuinServer = do
  liftIO $ updateGlobalLogger Server.logger (setLevel DEBUG)
  liftIO $ debugM Server.logger "running server.."
  return ()
  -}