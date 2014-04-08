{-# LANGUAGE OverloadedStrings #-}

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

-- internal module
import Utils

actionParam = "action"
hashParam = "hash"

archMagnet = "magnet:?xt=urn:btih:67f4bcecdca3e046c4dc759c9e5bfb2c48d277b0&dn=archlinux-2014.03.01-dual.iso&tr=udp://tracker.archlinux.org:6969&tr=http://tracker.archlinux.org:6969/announce"

data UTorrentConn = UTorrentConn { baseURL :: URL, user :: String, pass :: String, cookies :: CookieJar}
  deriving Show

data Torrent = Torrent [Value]
  deriving Show

data ProxyType = None | Socks4 | Socks5 | HTTPS | HTTP deriving (Enum, Show, Eq)

data ProxySetting = ProxySetType ProxyType | ProxyIP String | ProxyP2P Bool | ProxyPort Int
  deriving (Show, Eq)

getToken :: String -> String 
getToken = (\(TagText t) -> t) . (!! 2) . parseTags 

--"http://localhost:8080/gui"
-- get the connection
uTorentConn baseUrl user pass = do
  let url = (fromJust . importURL $ baseUrl) {url_path = "gui/"}
  let conn = UTorrentConn url user pass (createCookieJar [])
  res <- makeRequest $ conn {baseURL =  (baseURL conn) {url_path = "gui/token.html"}}
  liftIO $ P.putStrLn $ ("RESPONSE IS " ++) $  (show res)
  return $ UTorrentConn (add_param url ("token", getToken (DBSLC.unpack $ responseBody res)))
                        user pass (responseCookieJar res)


makeRequest conn = do
  --liftIO $ P.putStrLn $ show $ exportURL $ baseURL conn
  P.putStrLn $ exportURL $ baseURL $ conn
  request <- parseUrl $ exportURL $ baseURL conn
  completeReq <- return $ (applyBasicAuth (DBSC.pack $ user conn)
            (DBSC.pack $ pass conn) (request { cookieJar = Just $ cookies conn }))
  liftIO $ P.putStrLn $ show $  completeReq
  result <- withManager $ \manager -> do
    res <- httpLbs completeReq manager
    return  res
  return result    


requestWithParams conn params =fmap responseBody $ makeRequest conn
                  {baseURL = P.foldl (\u p -> add_param u p) (baseURL conn) params}
 

list conn = fmap ( Torrent . (\(Array a) -> DV.toList a) . fromJust . (Data.HashMap.Strict.lookup "torrents")
                              . fromJust . (\s -> decode s :: Maybe Object))
              $ requestWithParams conn [("list", "1")]

pause conn hash = requestWithParams conn [(hashParam, hash), (actionParam, "pause")]
addUrl conn url = requestWithParams conn [("s", url), (actionParam, "add-url")]

settingToParam (ProxySetType proxyType) = ("proxy.type", show . fromEnum $ proxyType)
settingToParam (ProxyIP ip) =  ("proxy.proxy", ip)
settingToParam (ProxyP2P isP2P) = ("proxy.p2p", show . fromBool $ isP2P)
settingToParam (ProxyPort port) = ("proxy.port", show port) 

setProxySettings :: UTorrentConn -> [ProxySetting] -> IO ()
setProxySettings conn settings =if' (settings == []) (return ()) $ do
  requestWithParams conn $ P.reverse $ (actionParam, "setsetting") :
        (join $ P.map ((\(s, v) -> [("s", s), ("v", v)]) . settingToParam) settings)
  return ()


-- toy main function just for testing;
-- TODO; throw away
main = do
  conn <- uTorentConn "http://localhost:8080" "admin" ""
  P.putStrLn "made first connection"
  r <- list conn
  P.putStrLn $ show $  r
  r2 <- addUrl conn archMagnet
  r3 <- setProxySettings conn [ProxySetType Socks4, ProxyIP "127.0.0.69", ProxyPort 6969, ProxyP2P True]
  P.putStrLn $ show $  r3
  return ()


{-

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