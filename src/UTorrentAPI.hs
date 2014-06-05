{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

module UTorrentAPI where
  
import Network.HTTP.Conduit
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as L
import Control.Monad.IO.Class (liftIO)
import Network.URL
import Data.Maybe
import Text.HTML.TagSoup
import Data.ByteString.Lazy.Char8 as DBSLC
import Prelude as P
import Data.ByteString.Char8 as DBSC
import Data.Aeson
import Data.HashMap.Strict
import Data.Text as DT
import Data.Vector as DV (toList)
import Foreign.Marshal.Utils as FMU
import Control.Monad
import Control.Exception.Lifted as CEL
import Control.Monad.Instances
import Control.Monad.Error as CME
import Control.Monad.Error.Class
import Data.String
import Control.Exception
import Control.Monad.Trans.Control

import System.Log.Logger
import System.Log.Handler.Syslog
import System.Log.Handler.Simple
import Network
import Network.Connection
import Network.Socket.Internal
import TorrentClient


-- internal module
import Utils

actionParam = "action"
hashParam = "hash"

logger = "fuin.utorrentapi"



data UTorrentConn = UTorrentConn { baseURL :: URL, user :: String, pass :: String, cookies :: CookieJar}
  deriving Show


makeUTorrentConn :: MakeTorrentClientConn
makeUTorrentConn hostName portNum (user, pass) = do
  conn <- uTorentConn (utServerURL hostName portNum) user pass
  return $ TorrentClientConn {addMagnetLink = addUrl conn, listTorrents = list conn,
                              pauseTorrent = pause conn, setProxySettings = setSettings conn,
                              connectPeer = addPeer conn}


-- refactor this crap based on the previous pattern
uTorentConn baseUrl user pass = do
  let url = (fromJust . importURL $ baseUrl) {url_path = "gui/"}
  let conn = UTorrentConn url user pass (createCookieJar [])
  res <- makeRequest $ conn {baseURL =  (baseURL conn) {url_path = "gui/token.html"}}
  liftIO $ debugM logger $ ("RESPONSE from utserver is " ++) $  (show res)
  return $ UTorrentConn (add_param url ("token", getToken (DBSLC.unpack $ responseBody res)))
                        user pass (responseCookieJar res)


--makeRequest :: (MonadTorrentClient m) => UTorrentConn -> m (Response L.ByteString)
makeRequest conn = do
  request <-liftIO $ parseUrl $ exportURL $ baseURL conn
  completeReq <- return $ (applyBasicAuth (DBSC.pack $ user conn)
            (DBSC.pack $ pass conn) (request { cookieJar = Just $ cookies conn }))
  potentialResult <- CEL.try (withManager $ \manager -> httpLbs completeReq manager)
  case potentialResult of
    Left (e :: SomeException) -> CME.throwError $ show e 
    Right r -> return r


requestWithParams conn params = fmap responseBody $ makeRequest conn
                  {baseURL = P.foldl (\u p -> add_param u p) (baseURL conn) params}
 
-- TODO: correctly implement this
list conn = undefined
{-
list conn = fmap ((P.map (Torrent . show) ) . (\(Array a) -> DV.toList a) . fromJust . (Data.HashMap.Strict.lookup "torrents")
                              . fromJust . (\s -> decode s :: Maybe Object))
              $ requestWithParams conn [("list", "1")]
-}
pause conn hash = requestWithParams conn [(hashParam, hash), (actionParam, "pause")] >> return ()
addUrl conn url = requestWithParams conn [("s", url), (actionParam, "add-url")] >> return ()
addPeer conn hash host port = undefined -- the utorrent server currently doesn't support this

settingToParam (ProxySetType proxyType) = ("proxy.type", show . fromEnum $ proxyType)
settingToParam (ProxyIP ip) =  ("proxy.proxy", ip)
settingToParam (ProxyP2P isP2P) = ("proxy.p2p", show . fromBool $ isP2P)
settingToParam (ProxyPort (PortNumber n)) = ("proxy.port", show n) 


--setProxySettings :: UTorrentConn -> [ProxySetting] -> IO ()
setSettings conn settings =if' (settings == []) (return ()) $ do
  requestWithParams conn $ P.reverse $ (actionParam, "setsetting") :
        (join $ P.map ((\(s, v) -> [("s", s), ("v", v)]) . settingToParam) settings)
  return ()

getToken :: String -> String 
getToken = (\(TagText t) -> t) . (!! 2) . parseTags 

utServerURL :: HostName -> PortNumber -> String
utServerURL hostName (PortNum p) = "http://" P.++ hostName P.++ ":" P.++ (show p)

{-
DEBUGGING code used for manual testing
TODO: remove when done

-}
runTorrentClientScript = do
  conn <- uTorentConn "http://localhost:8080" "admin" ""
  liftIO $ debugM logger "made first connection"
  r2 <- addUrl conn archMagnet
  liftIO $ debugM logger $ "addUrl RESPONSE IS " ++  (show r2)
  r <- list conn
--  liftIO $ debugM logger $ show r
  --r3 <- setProxySettings conn [ProxySetType Socks4, ProxyIP "127.0.0.69", ProxyPort 6969, ProxyP2P True]
  --liftIO $ debugM logger $ show $  r3
  return ()

archMagnet = "magnet:?xt=urn:btih:67f4bcecdca3e046c4dc759c9e5bfb2c48d277b0&dn=archlinux-2014.03.01-dual.iso&tr=udp://tracker.archlinux.org:6969&tr=http://tracker.archlinux.org:6969/announce"

runWithErrorHandling = do
  liftIO $ updateGlobalLogger logger (setLevel DEBUG)
  results <- runErrorT runTorrentClientScript
  P.putStrLn $ show results
  return ()

{-
  HTTP calls notes:

set settings calls

set proxy to socks5
http://localhost:8080/gui/?token=6pvMv-pFjjA6IcGkotXcW8hMfNwu5hPeBMLksFaQo_ACDFD8_N4yiHf0JFM=&action=setsetting&s=proxy.type&v=2

set port
http://localhost:8080/gui/?token=6pvMv-pFjjA6IcGkotXcW8hMfNwu5hPeBMLksFaQo_ACDFD8_N4yiHf0JFM=&action=setsetting&s=proxy.port&v=9998


set address
http://localhost:8080/gui/?token=6pvMv-pFjjA6IcGkotXcW8hMfNwu5hPeBMLksFaQo_ACDFD8_N4yiHf0JFM=&action=setsetting&s=proxy.proxy&v=127.0.1.1

set p2p proxy 
http://localhost:8080/gui/?token=6pvMv-pFjjA6IcGkotXcW8hMfNwu5hPeBMLksFaQo_ACDFD8_N4yiHf0JFM=&action=setsetting&s=proxy.p2p&v=0
-}