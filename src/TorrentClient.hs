{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module TorrentClient where

import Control.Monad.IO.Class
import Text.HTML.TagSoup
import Data.Aeson
import Control.Monad.Trans.Control
import Control.Monad.Error.Class
import Network
import Network.Connection
import Data.ByteString as DB
import Data.Word

{-
Defines the interface for interacting with a bittorrent client
-}


data ProxyType = None | Socks4 | Socks5 | HTTPS | HTTP deriving (Enum, Show, Eq)

data Setting = ProxySetType ProxyType | ProxyIP String | ProxyP2P Bool | ProxyPort Word16
               | DHTNetwork Bool | UTP Bool | PeerExchange Bool | LocalPeerDiscovery Bool
               | DHTForNewTorrents Bool | UPnP Bool | NATPMP Bool | RandomizePort Bool
               | BindPort Word16
  deriving (Show, Eq)

data Torrent = Torrent {torrentID :: String, torrentName :: String}
  deriving Show

type TorrentHash = String

type MonadTorrentClient m =  (MonadIO m, MonadError String m, Functor m, MonadBaseControl IO m)

-- user, passwd
type Credentials = (String, String)
type MakeTorrentClientConn = HostName -> Word16 -> Credentials -> InitTorrentClientConn
type InitTorrentClientConn = (MonadTorrentClient m) => m TorrentClientConn

data TorrentClientConn =  TorrentClientConn {
  addMagnetLink :: (MonadTorrentClient m) => String -> m TorrentHash,
  addTorrentFile :: (MonadTorrentClient m) => FilePath -> m TorrentHash,
  listTorrents :: (MonadTorrentClient m) => m [Torrent],
  pauseTorrent :: (MonadTorrentClient m) => TorrentHash -> m (),
  setSettings :: (MonadTorrentClient m) => [Setting] -> m (),
  connectPeer :: (MonadTorrentClient m) => TorrentHash -> HostName -> PortNumber -> m ()
                        }

type TorrentFileData = (FilePath, FilePath)

