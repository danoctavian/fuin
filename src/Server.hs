{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE  ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Server where

import Prelude as P
import Utils
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

import Encryption

logger = "fuin.server"

type MonadFuinServer m = (MonadIO m, MonadBaseControl IO m)
type HandleConnection = (MonadFuinServer m) => (Send, Receive) -> m ()

runReverseProxy :: (MonadIO m) => PortID  -> PortID -> InitHook -> m ()
runReverseProxy listenPort targetPort initHook
  = runServer (Config  listenPort initHook
            (\s -> return $ Connection CONNECT IPV4 $ SockAddrInet
                  (PortNum $ toggleEndianW16 ((\(PortNumber n) -> fromIntegral n :: Word16) targetPort))
                   $ readIPv4 "127.0.0.1"))

-- TODO: figure out port/port-range for which to reverse proxy
-- need to know how bittorrent finds ot where to connect to a certain peer (DHT?)
run :: (MonadFuinServer m) => HandleConnection -> BootstrapServerEncryption -> m ()
run handleConnection bootstrapEnc = do
  liftIO $ forkIO $ runReverseProxy (PortNumber 6881) (PortNumber 3333) $
                    serverReverseProxyInit handleConnection bootstrapEnc -- magic ports; standard bittorrent port + magic bittorrent client port
  return ()



serverReverseProxyInit :: HandleConnection -> BootstrapServerEncryption -> InitHook
serverReverseProxyInit handleConnection bootstrapEnc clientSock serverSock = do
  [incomingPipe, outgoingIn, outgoingOut] <- replicateM 3 (liftIO $ atomically newTChan)
  let outgoingPipe = (outgoingIn, outgoingOut)
  [sendChan, receiveChan] <- replicateM 2 (liftIO $ atomically newTChan)

  return $ PacketHandlers {
   -- incoming packets are packets from the Bittorrent reverse proxied server
    incoming = (pieceHandler $ transformPacket outgoingPipe),
    outgoing = (pieceHandler $ collectPacket incomingPipe)
    -- outgoing packets are packets send by the connection initiator, which is the client
  }
{-
init: start reverse proxy
<no torrent client settings atm>
give connection handler (some function to call when a connection is made)

in init - need to check that the connection is the type we wanted
  technique: attempt decryption; if no packets succeed after n packs make it a normal connection
  for this spawn a thread  to handle this crap


  seems simpler than the client
-}

runFuinServer :: (MonadIO m) => m ()
runFuinServer = do
  liftIO $ updateGlobalLogger Server.logger (setLevel DEBUG)
  liftIO $ debugM Server.logger "running server.."
  return ()