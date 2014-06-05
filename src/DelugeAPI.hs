{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module DelugeAPI where

import Network.HTTP.Conduit
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as L
import Control.Monad.IO.Class (liftIO)
import Network.URL
import Data.Maybe
import Text.HTML.TagSoup
import Data.ByteString.Lazy.Char8 as DBLC
import Prelude as P
import Data.ByteString.Char8 as DBC
import Data.ByteString as DB
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
import Network.Socket as NS
import Network.Socket.ByteString as NSB
import Network.TLS as TLS
import Control.Concurrent
import Network.Connection as NC
import System.Random  as SR

import Data.Map as DM
import Data.Serialize as DS
import Data.Attoparsec as DA
import Data.Default

import Codec.Compression.Zlib as CCZ
import Utils

import TorrentClient
import REncode



makeDelugeConn :: MakeTorrentClientConn
makeDelugeConn hostName portNum (user, pass) = do
  ctx <- liftIO $ initConnectionContext
  conn <- liftIO $ NC.connectTo ctx $ ConnectionParams
                            { connectionHostname  = hostName
                            , connectionPort      = portNum
                            , connectionUseSecure = Just (def {settingDisableCertificateValidation = True})
                            , connectionUseSocks  = Nothing
                            } 

  stdGen <- liftIO $ getStdGen
  let dConn = DelugeConn conn stdGen
  sendMsg dConn $ login user pass
  return $ TorrentClientConn {
                            addMagnetLink = addLink dConn,
                            listTorrents = list dConn,
                            pauseTorrent = pause dConn,
                            setProxySettings = proxySettings dConn,
                            connectPeer = addPeer
                        }

data DelugeConn = DelugeConn Connection StdGen
data DelugeResponse = SessionState [DB.ByteString] | TorrentStatus {tName :: DB.ByteString} | DelugeError String | Success
                    | CurrentConfig | Plain REncode
  deriving (Show, Eq)

data DelugeCmd = DelugeCmd {cmdReq :: Integer -> REncode, cmdResp :: REncode -> DelugeResponse}

addLink delugeConn link = undefined
list delugeConn = do
   (Right (SessionState tids)) <- sendMsg delugeConn sessionState
   statuses <- forM tids $ (sendMsg delugeConn) . torrentStatus 
   return $ P.map(\(tid, (Right stat)) -> 
                  Torrent (DBC.unpack tid) (DBC.unpack $ tName stat))
          $ P.zip tids statuses
pause delugeConn hash = undefined
proxySettings delugeConn settings = undefined
addPeer torrentHash ip port = undefined

login user password =
  DelugeCmd (msg "daemon.login" [RString $ DBC.pack user, RString $ DBC.pack password] DM.empty)
  (\r -> Success)
info = DelugeCmd (msg "daemon.info" [] DM.empty) (\r -> Success)
currentConfig = DelugeCmd (msg "core.get_config" [] DM.empty) (\r -> CurrentConfig)
sessionState = DelugeCmd (msg "core.get_session_state" [] DM.empty)
               (\(RList ids) -> SessionState $ P.map (\(RString s) -> s) ids)
torrentStatus torrentId
  = DelugeCmd (msg "core.get_torrent_status" [RString torrentId, RList [RString "name"], RBool False] DM.empty)
    (\(RDict mp) -> TorrentStatus $ (\(RString s) -> s) $ fromJust $ DM.lookup (RString "name") mp)

sendMsg (DelugeConn conn stdGen) msg = do
  randId <- liftIO $ fmap abs (randomIO :: IO Integer)
  liftIO $ connectionPut conn $ serializeMsg $ cmdReq msg randId 
  response <- liftIO $ connectionGet conn 2048
  return . (fmap $ (cmdResp msg) . unwrapResp) . (parseOnly rEncodeParser) .
    toStrict . CCZ.decompress . (DBLC.fromChunks . (:[])) $ response

unwrapResp (RList [i, msgId, resp]) = resp

msg method params kvParams msgId = RList [RInt msgId, RString method, RList params, RDict kvParams]
-- encodes and compresses
serializeMsg m =  toStrict $ CCZ.compress $ DBLC.fromChunks $ (:[]) $ DS.encode $ RList [m]


testDriveDelugeConn :: (MonadTorrentClient m) => m ()
testDriveDelugeConn = do
  dc <- makeDelugeConn "localhost" 58846 ("boss", "pass")
  leList <- listTorrents dc
  liftIO $ P.putStrLn $ show leList
  return ()

runTestDrive :: IO ()
runTestDrive = do
  runErrorT testDriveDelugeConn
  return ()


-- tls fuckaround
tryTls = do
    ctx <- initConnectionContext
    con <- NC.connectTo ctx $ ConnectionParams
                            { connectionHostname  = "localhost"
                            , connectionPort      = 58846
                            , connectionUseSecure = Just (def {settingDisableCertificateValidation = True})
                            , connectionUseSocks  = Nothing
                            }

    connectionPut con $ serializeMsg $ RList [RInt 456, RString $ DBC.pack "daemon.login", RList [RString "boss", RString "pass"], RDict DM.empty]
     -- wait for a reply and print
    --liftIO $ threadDelay $ 10 ^ 6 * 2
    readMsg con
    connectionPut con $ serializeMsg $  RList [RInt 13, RString $ DBC.pack "core.get_config", RList [], RDict DM.empty]
    readMsg con
    connectionClose con 

readMsg con = do
  line <- connectionGet  con 1024
  P.putStrLn . show . (parseOnly rEncodeParser) . toStrict . CCZ.decompress . (DBLC.fromChunks . (:[])) $ line


{-
- torrent status params

"save_path",
"tracker",
"next_announce",
"name",
"total_size",
"progress",
"num_seeds",
"total_seeds",
"num_peers",
"total_peers",
"eta",
"download_payload_rate",
"upload_payload_rate",
"ratio",
"distributed_copies",
"num_pieces",
"piece_length",
"total_done",
"files",
"file_priorities",
"file_progress",
"peers",
"is_seed",
"is_finished"
-}