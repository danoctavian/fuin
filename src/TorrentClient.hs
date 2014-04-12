module TorrentClient where

import Control.Monad.IO.Class
import Text.HTML.TagSoup
import Data.Aeson


data ProxyType = None | Socks4 | Socks5 | HTTPS | HTTP deriving (Enum, Show, Eq)

data ProxySetting = ProxySetType ProxyType | ProxyIP String | ProxyP2P Bool | ProxyPort Int
  deriving (Show, Eq)

data Torrent = Torrent [Value]
  deriving Show

type TorrentHash = String

class TorrentClientConn c where
  add :: (MonadIO m) => c -> String -> m () 
  listTorrents :: (MonadIO m) => c -> m [Torrent]
  pauseTorrent :: (MonadIO m) => c -> TorrentHash -> m [Torrent]