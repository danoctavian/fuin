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
import Control.Concurrent
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



type MonadFuinClient m = (MonadIO m, MonadBaseControl IO m)
 
-- constants
logger = "fuin.client"  
btConnStartTimeout = 10 * 15 ^ 6 -- microseconds

init :: (MonadFuinClient m, MonadError String m) => PortID -> MakeTorrentClientConn -> m (Transporter)
init port makeTorrentConn = do
  liftIO $ debugM Client.logger "initializing..."

  addressDict <- liftIO $ newTVarIO DM.empty
  liftIO $ forkIO $ Socks5Proxy.runServer $ Config {listenPort = port, initHook = clientSocks5Init addressDict,
                                    getConn = doSocksHandshake}

  torrentClient <- makeTorrentConn
  setProxySettings torrentClient [ProxySetType Socks4, ProxyIP localhost, ProxyPort $ port, ProxyP2P True]
  
  {-
  <startUTorrentServer>
  -- connect
  conn <- uTorrentConn "http://localhost:8080" "admin" ""
  response <- setProxySettings conn 
  -}
  return $ Transporter {makeConn = makeConnection torrentClient addressDict}


makeConnection :: (MonadFuinClient m, MonadError String m) =>
                  TorrentClientConn -> TVar AddressDict -> ServerInfo -> m (Send, Receive)
makeConnection torrentClient addressDict serverInfo = do
  liftIO $ debugM Client.logger $ "make connection" ++ (show $ serverSockAddr serverInfo)

  -- write to tvar the new address that needs to be caught along with then channels to be picked up and used
  -- create data channels
  [incomingPipe, outOutgoing] <- replicateM 2 (liftIO $ atomically newTChan)
  inOutgoing <- liftIO $ atomically newTChan
  control <- liftIO $ atomically newTChan
  liftIO $ atomically $  modifyTVar addressDict
                      (insert (serverSockAddr serverInfo) (control, incomingPipe, (inOutgoing, outOutgoing)))

  -- let the games begin 
  addMagnetLink torrentClient (serverTorrentFile serverInfo)
  ping <- liftIO $ timeout btConnStartTimeout (liftIO $ atomically $ readTChan control)
  when (ping == Nothing) $ throwError "bittorrent connection start timeout"

  [send, receive] <- replicateM 2 (liftIO $ atomically newTChan)
  let encryption = clientEncyption serverInfo
  -- make message consuming threads
  -- outgoing : read message chan ; produce byte string blocks 
  liftIO $ forkIO $ streamIncoming receive incomingPipe $ clientDecrypt encryption
  -- incoming : read bytestring - parse and push into message chan 
  -- TODO; add the bootstrap encryption and then switch from it 
  liftIO $ forkIO $ streamOutgoing send (inOutgoing, outOutgoing) $ clientEncrypt encryption

  sendMessage send $ ClientGreeting $ bootstrapData encryption

  return (sendMessage send,  receiveMessage receive)



clientSocks5Init :: TVar AddressDict -> InitHook
clientSocks5Init addresses clientSock serverSock = do
  liftIO $ debugM Client.logger $ "running initialization in socks5"
  addressDict <- liftIO $ readTVarIO addresses
  liftIO $ debugM Client.logger $ "socks5 proxy is meant to connect to "
        P.++ (show serverSock) P.++ " while the address record contains " P.++ (show $ keys addressDict)
  case DM.lookup serverSock addressDict of
    Nothing -> do
      liftIO $ debugM Client.logger $ "new socks5 connection is not a fuin connection"
      return $ idPacketHandlers
    Just (control, inChan, outgoingPipe)
      -> do
        liftIO $ atomically $ writeTChan control Ping -- tell the owner of init that it has run
        return $ PacketHandlers
          (pieceHandler $ collectPacket inChan)
          (pieceHandler $ transformPacket outgoingPipe)




runClient :: (MonadIO io) => io ()
runClient = do
    liftIO $ updateGlobalLogger Client.logger (setLevel DEBUG)

   -- makeConnection transporter $ ServerAddress (1 :: Word32) "muelagabori" $ SockAddrInet (portNumberle 6000) (1 :: Word32)
    return ()
