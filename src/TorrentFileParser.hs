{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module TorrentFileParser where

import Data.BEncode
import Data.BEncode.Parser
import Data.Digest.SHA1 as SHA1
import Data.Maybe
import Data.Binary
import Data.Generics
import Data.List as DL

import Prelude as P
import qualified Data.ByteString.Char8 as DBC
import qualified Data.ByteString.Lazy.Char8 as DBCL
import Data.ByteString.Lazy as DBL
import Data.ByteString.Lazy
import Data.ByteString.Lazy (fromChunks)
import Data.ByteString as DB

import Control.Applicative as CA ((<$>))
import Control.Monad

import Utils

import qualified Data.Map as Map

data Torrent
    = Torrent
    { tAnnounce     :: DBL.ByteString
    , tAnnounceList :: [DBL.ByteString]
    , tComment      :: DBL.ByteString
    , tCreatedBy    :: Maybe DBL.ByteString
    , tInfo         :: TorrentInfo
    , tInfoHash     :: DBL.ByteString
    } deriving (Show, Read, Typeable, Data)

data TorrentInfo
    = SingleFile
    { tLength      :: Int
    , tName        :: DBL.ByteString
    , tPieceLength :: Int
    , tPieces      :: DBL.ByteString }
    | MultiFile
    { tFiles       :: [TorrentFile]
    , tName        :: DBL.ByteString
    , tPieceLength :: Int
    , tPieces      :: DBL.ByteString
    } deriving (Show, Read, Typeable, Data)

data TorrentFile
    = TorrentFile
    { fileLength :: Int
    , filePath   :: [DBL.ByteString]
    } deriving (Show, Read, Typeable, Data)

instance Binary Torrent where
    put = put . serializeTorrent
    get = do e <- get
             case readTorrent e of
               Left err -> fail $ "Failed to parse torrent: " ++ err
               Right t  -> return t


torrentSize :: Torrent -> Int
torrentSize torrent
    = case tInfo torrent of
        s@SingleFile{} -> tLength s
        MultiFile{tFiles=files} -> P.sum (P.map fileLength files)

{-
buri :: BParser BEncode -> BParser URI
buri p = do str <- bstring p
            case parseURI str of
              Nothing -> fail $ "Expected URI: " ++ str
              Just uri -> return uri
-}

readTorrent :: DBL.ByteString -> Either String Torrent
readTorrent inp
    = case bRead inp of
        Nothing -> Left "Not BEncoded"
        Just be -> runParser parseTorrent be

parseTorrent :: BParser Torrent
parseTorrent =
    do announce <- bbytestring $ dict "announce"
       creator  <- optional $ bbytestring $ dict "created by"
       info     <- dict "info"
       setInput info
       name     <- bbytestring $ dict "name"
       pLen     <- bint $ dict "piece length"
       pieces   <- bbytestring $ dict "pieces"
       torrentInfo      <-  parseTorrentInfo (P.id name) (fromIntegral pLen) (P.id pieces)

       -- TODO: fix infoHash -> DBL.ByteString. replace the dummy "infoHash" below
       let infoHash = SHA1.hash (DBL.unpack $ DBCL.pack $ bShow info "")
       return $ Torrent announce [] DBCL.empty creator torrentInfo "infoHash"

parseTorrentInfo :: DBL.ByteString -> Int -> DBL.ByteString -> BParser TorrentInfo
parseTorrentInfo name pLen pieces
    = do len <- fmap fromIntegral $ bint $ dict "length"
         return $ SingleFile len name pLen pieces
      <|>
      do files <- list "files" $ do len <- fmap fromIntegral $ bint $ dict "length"
                                    filePaths <- list "path" $ bbytestring token
                                    return $ TorrentFile len (P.map P.id filePaths)
         return $ MultiFile files name pLen pieces

serializeTorrent :: Torrent -> BEncode
serializeTorrent torrent
    = BDict $ Map.fromList [("announce",BString $ tAnnounce torrent)
                           ,("comment", BString $ tComment torrent)
                           ,("info",info)
                           ]
      where info = BDict $ Map.fromList $ [("name", BString $ tName (tInfo torrent))
                                          ,("pieces", BString $ tPieces (tInfo torrent))
                                          ,("piece length", BInt $ fromIntegral $ tPieceLength (tInfo torrent))
                                          ] ++ case tInfo torrent of
                                                SingleFile len _ _ _ -> [("length", BInt $ fromIntegral len)]
                                                MultiFile files _ _ _ -> [("files", BList $ flip P.map files $ \file ->
                                                                               BDict $ Map.fromList [("length", BInt $ fromIntegral (fileLength file))
                                                                                                    ,("path", BList (P.map BString $ filePath file))]
                                                                          )]

-- input is assumed correct
getPiece :: Int -> DBL.ByteString -> Int -> DBL.ByteString
getPiece pieceSize bs index
  = let sz = fromIntegral pieceSize in 
    DBL.take sz $  DBL.drop ((fromIntegral index) * sz) bs

-- assumes input is correct
pieceLoader torrentFilePath filePath = do
  torrent <- (fromRight . readTorrent) <$> DBL.readFile torrentFilePath
  let (pieceLength, totalLength) = pieceSizing torrent
  file <- DBL.readFile filePath
  return $ getPiece pieceLength file 

pieceSizing t = let info = tInfo t in (tPieceLength info, tLength info)


data Halves = FstHalf | SndHalf
selectHalf torrentFile file half =
  let halfLen = pieceLen * ((totalLen `div` pieceLen) `div` 2)
      (pieceLen, totalLen) = pieceSizing torrentFile in
  case half of
    FstHalf -> DB.concat [DB.take halfLen file, DB.replicate (totalLen - halfLen) 0]
    SndHalf -> DB.concat [DB.replicate halfLen 0, DB.drop halfLen file]


makeHalfFile torrentFile original new half = do
  torrent <- (fromRight . readTorrent) <$> DBL.readFile torrentFile
  originalData <- DB.readFile original
  DB.writeFile new (selectHalf torrent originalData half)


pieceBlock piece begin len =DBL.take len $ DBL.drop begin $ piece

toGetPieceBlock getPiece index = pieceBlock (getPiece index) 

testOnRealFile = do
  tFile <- DBL.readFile "/home/dan/test/smallFile.dan.torrent"
  P.putStrLn $ show (readTorrent tFile)


testLoadPieces = do
  getPiece <- pieceLoader "/home/dan/test/bigFile.dan.torrent" "/home/dan/test/bigFile.dan"
  getPieceP <- pieceLoader "/home/dan/test/bigFile.dan.torrent" "/home/dan/tools/utorrent-server-v3_0/bigFile.dan"
  P.putStrLn $ show $ DBL.all (== 0) $ getPieceP 0
  --forM [1..30] $ \i -> P.putStrLn $ show $ (getPiece i) == (getPieceBroken i)
  return ()
  --P.putStrLn $ show $ DBL.length $ getPieceBroken 17

makeHalves = do
  --makeHalfFile "/home/dan/test/bigFile.dan.torrent" "/home/dan/test/bigFile.dan" "/home/dan/test/fstHalfbigFile.dan"  FstHalf
  makeHalfFile "/home/dan/test/bigFile.dan.torrent" "/home/dan/test/bigFile.dan" "/home/dan/test/sndHalfbigFile.dan"  SndHalf
  return ()`