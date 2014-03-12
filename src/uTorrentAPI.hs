{-# LANGUAGE OverloadedStrings #-}
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
--main = simpleHttp "http://www.haskell.org/" >>= L.putStr

data UTorrentConn = UTorrentConn { baseURL :: URL, user :: String, pass :: String, cookies :: CookieJar}
  deriving Show



data Foo = Foo {bar :: Int, lol :: Int}
  deriving Show

getToken :: String -> String 
getToken = (\(TagText t) -> t) . (!! 2) . parseTags 

--"http://localhost:8080/gui"
-- get the connection
--uTorentConn :: String -> String -> String -> IO (UTorrentConn)
uTorentConn baseUrl user pass = do
  let url = (fromJust . importURL $ baseUrl) {url_path = "gui/"}
  let conn = UTorrentConn url user pass (createCookieJar [])
  res <- makeRequest $ conn {baseURL =  (baseURL conn) {url_path = "gui/token.html"}}
 -- liftIO $ P.putStrLn $ ("cookies " ++) $ show $ responseCookieJar res
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


requestWithURL conn urlTrans = makeRequest conn {baseURL = urlTrans (baseURL conn)}
 

main = do
  conn <- uTorentConn "http://localhost:8080" "admin" ""
  --P.putStrLn $ exportURL $ baseURL $ conn
  --r <- makeRequest $ conn P{baseURL = (baseURL conn) {url_path = ""}}
  P.putStrLn "made first connection"
  r <- requestWithURL conn (\url -> add_param url ("list", "1"))
  P.putStrLn $ show r
  return ()
