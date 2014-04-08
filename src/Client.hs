{-# LANGUAGE OverloadedStrings #-}

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

data Transporter = Transporter UTorrentAPI.UTorrentConn

type Key = Word32

clientLogger = "fuin.client"  

data ServerAddress = ServerAddress Key
  deriving Show
init ::(MonadIO m) => PortID -> m ()
init port = do
  liftIO $ debugM clientLogger "initializing..."
  {-}
  liftIO $ Socks5Proxy.runServer $ Config {getPort = port,
                                    handleIncoming = (justPrint "incoming"), handleOutgoing = (justPrint "outgoing")}
-}
  {-
  <startUTorrentServer>
  -- connect
  conn <- uTorrentConn "http://localhost:8080" "admin" ""
  response <- setProxySettings conn [ProxySetType Socks4, ProxyIP localhost, ProxyPort port, ProxyP2P True]
  -}


makeConnection :: (MonadIO io) => Transporter -> ServerAddress -> io ()
makeConnection trans serverAddr = do
  liftIO $ debugM clientLogger $ "make connection" ++ (show serverAddr)
  return ()

runClient :: (MonadIO io) => io ()
runClient = do
    liftIO $ updateGlobalLogger "fuin.client" (setLevel DEBUG)
--    s <-liftIO $ streamHandler stdout DEBUG
--    liftIO $ updateGlobalLogger rootLoggerName (addHandler s)
    transporter <- liftIO $ Client.init $ PortNumber 1080
    return ()
