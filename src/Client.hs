{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}

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

data Transporter = Transporter UTorrentAPI.UTorrentConn

type Key = Word32

type TorrentFile = String

-- server public key; support file; IP 
data ServerAddress = ServerAddress Key TorrentFile SockAddr
  deriving Show


data Message = Data ByteString | SwitchChannel TorrentFile
type Send = (MonadIO m) => Message -> m ()
type Receive = (MonadIO m) => m Message

clientLogger = "fuin.client"  

init ::(MonadIO m) => PortID -> m Transporter
init port = do
  liftIO $ debugM clientLogger "initializing..."
  {-
  liftIO $ Socks5Proxy.runServer $ Config {getPort = port,
                                    handleIncoming = (justPrint "incoming"), handleOutgoing = (justPrint "outgoing")}
-}
  {-
  <startUTorrentServer>
  -- connect
  conn <- uTorrentConn "http://localhost:8080" "admin" ""
  response <- setProxySettings conn [ProxySetType Socks4, ProxyIP localhost, ProxyPort port, ProxyP2P True]
  -}
  return undefined


makeConnection :: (MonadIO m) => Transporter -> ServerAddress -> m (Send, Receive)
makeConnection trans serverAddr = do
  liftIO $ debugM clientLogger $ "make connection" ++ (show serverAddr)
  return undefined

runClient :: (MonadIO io) => io ()
runClient = do
    liftIO $ updateGlobalLogger "fuin.client" (setLevel DEBUG)
--    s <-liftIO $ streamHandler stdout DEBUG
--    liftIO $ updateGlobalLogger rootLoggerName (addHandler s)
    transporter <- Client.init $ PortNumber 1080
    makeConnection transporter $ ServerAddress (1 :: Word32) "muelagabori" $ SockAddrInet (portNumberle 6000) (1 :: Word32)
    return ()
