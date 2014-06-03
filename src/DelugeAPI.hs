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


import Codec.Compression.Zlib

import TorrentClient


{-
type MakeTorrentClientConn = (MonadTorrentClient m) => m (TorrentClientConn)

data TorrentClientConn =  TorrentClientConn {
                            addMagnetLink :: (MonadTorrentClient m) => String -> m (),
                            listTorrents :: (MonadTorrentClient m) => m [Torrent],
                            pauseTorrent :: (MonadTorrentClient m) => TorrentHash -> m (),
                            setProxySettings :: (MonadTorrentClient m) => [ProxySetting] -> m ()
                        }
-}

{-
message structure
[request_id, method, args, kwargs]

method = {daemon.info..}
-}

makeDelugeConn :: String -> String -> MakeTorrentClientConn 
makeDelugeConn addr port = undefined
