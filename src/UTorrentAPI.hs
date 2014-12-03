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
import Data.ByteString.Char8 as DBC
import Data.Aeson
import Data.HashMap.Strict
import Data.Text as DT
import Data.Vector as DV (toList, (!))
import Foreign.Marshal.Utils as FMU
import Control.Monad
import Control.Exception.Lifted as CEL
import Control.Monad.Instances
import Control.Monad.Error as CME
import Control.Monad.Error.Class
import Data.String
import Control.Exception
import Control.Monad.Trans.Control
import Data.ByteString as DB
import Network.HTTP.Client.MultipartFormData

import System.Log.Logger
import System.Log.Handler.Syslog
import System.Log.Handler.Simple
import Network
import Network.Connection
import Network.Socket.Internal
import TorrentClient
import Data.Text as DT
import System.FilePath.Posix
import Data.Word

-- internal module
import Utils

actionParam = "action"
hashParam = "hash"

logger = "fuin.utorrentapi"



{--TODO: this whole module is messed up - requests should be build up in different functions from 
 performing IO building requests is a pure thing -}

data UTorrentConn = UTorrentConn { baseURL :: URL, user :: String, pass :: String, cookies :: CookieJar}
  deriving Show


makeUTorrentConn :: MakeTorrentClientConn
makeUTorrentConn hostName portNum (user, pass) = do
  liftIO $ debugM UTorrentAPI.logger $ "attempting connection to " ++ (show $ utServerURL  hostName portNum)
  conn <- uTorentConn (utServerURL hostName portNum) user pass
  return $ TorrentClientConn {addMagnetLink = addUrl conn, listTorrents = list conn,
                              pauseTorrent = pause conn, setSettings = setSett conn,
                              connectPeer = addPeer conn,
                              addTorrentFile = addFile conn}


-- refactor this crap based on the previous pattern
uTorentConn baseUrl user pass = do
  let url = (fromJust . importURL $ baseUrl) {url_path = "gui/"}
  let conn = UTorrentConn url user pass (createCookieJar [])
  res <- makeRequest  conn (\url -> url {url_path = "gui/token.html"}) return
  liftIO $ debugM logger $ ("RESPONSE from utserver is " ++) $  (show res)
  return $ UTorrentConn (add_param url ("token", getToken (DBSLC.unpack $ responseBody res)))
                        user pass (responseCookieJar res)



--makeRequest :: (MonadTorrentClient m) => UTorrentConn -> m (Response L.ByteString)
makeRequest conn urlChange reqChange = do
  request <-liftIO $ parseUrl $ exportURL $ urlChange $ baseURL conn
  completeReq <- reqChange $ (applyBasicAuth (DBC.pack $ user conn)
            (DBC.pack $ pass conn) (request { cookieJar = Just $ cookies conn }))
  liftIO $ debugM logger (show completeReq)
  potentialResult <- CEL.try (withManager $ \manager -> httpLbs completeReq manager)
  case potentialResult of
    Left (e :: SomeException) -> CME.throwError $ show e 
    Right r -> return r


requestWithParams conn params reqChange = fmap responseBody $ makeRequest conn
                  (\url -> P.foldl (\u p -> add_param u p) url params) reqChange

 

 -- TODO: use lens here?
-- TODO: correctly implement this
list conn
  = fmap ((P.map (\(Array a) ->
      Torrent (DT.unpack $ fromAesonStr $ a DV.! 0) (DT.unpack $ fromAesonStr $ a DV.! 2)) )
      . (\(Array a) -> DV.toList a) . fromJust . (Data.HashMap.Strict.lookup "torrents")
      . fromJust . (\s -> decode s :: Maybe Object))
      $ requestWithParams conn [("list", "1")] return

pause conn hash = requestWithParams conn [(hashParam, hash), (actionParam, "pause")] return >> return ()
addUrl conn url = requestWithParams conn [("s", url), (actionParam, "add-url")] return  >> return url
addPeer conn hash host port = undefined -- the utorrent server currently doesn't support this
addFile conn filePath = requestWithParams conn [(actionParam, "add-file")]
                        (formDataBody[partFile "torrent_file" filePath])
                        >>= (return . show)


settingToParam (ProxySetType proxyType) = ("proxy.type", show . fromEnum $ proxyType)
settingToParam (ProxyIP ip) =  ("proxy.proxy", ip)
settingToParam (ProxyP2P isP2P) = ("proxy.p2p", boolSetting isP2P)
settingToParam (ProxyPort n) = ("proxy.port", show n) 
settingToParam (DHTNetwork b) = ("dht", boolSetting b) 
settingToParam (UTP b) = ("bt.transp_disposition", boolSetting b) 
settingToParam (PeerExchange b) = ("pex", boolSetting b) 
settingToParam (LocalPeerDiscovery b) = ("lsd", boolSetting b) 
settingToParam (DHTForNewTorrents b) = ("dht_per_torrent", boolSetting b) 
settingToParam (UPnP b) = ("upnp", boolSetting b) 
settingToParam (NATPMP b) = ("natpmp", boolSetting b) 
settingToParam (RandomizePort b) = ("rand_port_on_start", boolSetting b)
settingToParam (BindPort n) = ("bind_port", show n)

boolSetting b = show $ (fromBool $ b :: Int) 

fromAesonStr (String s) = s

setSett conn settings =if' (settings == []) (return ()) $ do
  requestWithParams conn (P.reverse $ (actionParam, "setsetting") :
        (join $ P.map ((\(s, v) -> [("s", s), ("v", v)]) . settingToParam) settings)) return
  return ()

getToken :: String -> String 
getToken = (\(TagText t) -> t) . (!! 2) . parseTags 

utServerURL :: HostName -> Word16 -> String
utServerURL hostName p = "http://" P.++ hostName P.++ ":" P.++ (show p)

{-
DEBUGGING code used for manual testing
TODO: remove when done
"http://127.0.0.1:8000"
-}
runTorrentClientScript = do
  conn <- makeUTorrentConn "127.0.0.1" 8000  ("admin", "")
  liftIO $ debugM logger "made first connection"
  r3 <- setSettings conn [UPnP False, NATPMP False, RandomizePort False, DHTForNewTorrents False, UTP True, LocalPeerDiscovery False, ProxySetType Socks4, ProxyIP "127.0.0.1", ProxyPort 1080, ProxyP2P True]
  liftIO $ debugM logger $ show $  r3
  torrentFile <-return "/home/dan/testdata/sample100.torrent"
--  r2 <- addMagnetLink conn archMagnet
  r2 <- addTorrentFile conn torrentFile 
  liftIO $ debugM logger $ "addUrl RESPONSE IS " ++  (show r2)
  r <- listTorrents conn
  liftIO $ debugM logger $ "list is  " ++  (show r)
--  liftIO $ debugM logger $ show r
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
