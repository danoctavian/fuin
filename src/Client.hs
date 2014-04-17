{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE  ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Client where

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
import Data.Map as DM
import System.Timeout

import TorrentClient
import PackageStream

import Encryption

data Transporter = Transporter {
                  makeConn :: (MonadFuinClient m, MonadError String m) => ServerInfo -> m ((Send, Receive))
                }


type EncryptF = ByteString -> ByteString

-- encrpytion - encrypt first hand; encrypt/decrypt afterwards


-- pipes for sending message to the server
-- and receiving


data Ping = Ping
  deriving (Eq)
type ControlChan = TChan Ping

type AddressDict = Map SockAddr (ControlChan, IncomingPipe, OutgoingPipe)
-- server public key; support file; IP 
data ServerInfo = ServerInfo {
                    clientEncyption :: ClientEncryption,
                    serverTorrentFile :: TorrentFile,
                    serverSockAddr :: SockAddr}


type Send = (MonadIO m) => Message -> m ()
type Receive = (MonadIO m) => m Message

type MonadFuinClient m = (MonadIO m, MonadBaseControl IO m)
 
-- constants
logger = "fuin.client"  
btConnStartTimeout = 5 * 10 ^ 6 -- microseconds

init :: (MonadFuinClient m, MonadError String m) => PortID -> MakeTorrentClientConn -> m (Transporter)
init port makeTorrentConn = do
  liftIO $ debugM Client.logger "initializing..."
  {-
  liftIO $ Socks5Proxy.runServer $ Config {getPort = port,
                                    handleIncoming = (justPrint "incoming"), handleOutgoing = (justPrint "outgoing")}
-}

  torrentClient <- makeTorrentConn
  setProxySettings torrentClient [ProxySetType Socks4, ProxyIP localhost, ProxyPort $ port, ProxyP2P True]
  
  addressDict <- liftIO $ newTVarIO DM.empty
  {-
  <startUTorrentServer>
  -- connect
  conn <- uTorrentConn "http://localhost:8080" "admin" ""
  response <- setProxySettings conn 
  -}
  return $ Transporter {makeConn = makeConnection torrentClient addressDict}


makeConnection :: (MonadFuinClient m, MonadError String m) =>
                  TorrentClientConn -> TVar AddressDict -> ServerInfo -> m (Send, Receive)
makeConnection torrentClient addressDict serverAddr = do
  liftIO $ debugM Client.logger $ "make connection" ++ (show $ serverSockAddr serverAddr)
  --TODO
  -- write to tvar the new address that needs to be caught along with then channels to be picked up and used

  -- create data channels
  [incomingPipe, inOutgoing, outOutgoing] <- replicateM 3 (liftIO $ atomically newTChan)
  control <- liftIO $ atomically newTChan
  liftIO $ atomically $  modifyTVar addressDict
                      (insert (serverSockAddr serverAddr) (control, incomingPipe, (inOutgoing, outOutgoing)))

  -- let the games begin 
  addMagnetLink torrentClient (serverTorrentFile serverAddr)
  ping <- liftIO $ timeout btConnStartTimeout (liftIO $ atomically $ readTChan control)
  when (ping == Nothing) $ throwError "bittorrent connection start timeout"

  -- make message consuming threads
  -- outgoing : read message chan ; produce byte string blocks 
  -- incoming : read bytestring - parse and push into message chan 
  return undefined


clientSocks5Init :: TVar AddressDict -> InitHook
clientSocks5Init addresses clientSock serverSock = do
  liftIO $ debugM Client.logger $ "running initialization in socks5"
  addressDict <- liftIO $ readTVarIO addresses
  case DM.lookup serverSock addressDict of
    Nothing -> return $ idPacketHandlers
    Just (control, inChan, outgoingPipe)
      -> do
        liftIO $ atomically $ writeTChan control Ping -- tell the owner of init that it has run
        return $ PacketHandlers
          (pieceHandler $ clientIncoming inChan)
          (pieceHandler $ clientOutgoing outgoingPipe)

clientIncoming chan bs = do 
  liftIO $ atomically $ writeTChan chan bs
  return bs 

clientOutgoing (inChan, outChan) bs = do
  liftIO $ atomically $ writeTChan inChan bs
  liftIO $ atomically $ readTChan outChan


runClient :: (MonadIO io) => io ()
runClient = do
    liftIO $ updateGlobalLogger Client.logger (setLevel DEBUG)
  --  transporter <- Client.init $ PortNumber 1080
   -- makeConnection transporter $ ServerAddress (1 :: Word32) "muelagabori" $ SockAddrInet (portNumberle 6000) (1 :: Word32)
    return ()
