{-# LANGUAGE OverloadedStrings #-}
import Prelude as P
import Data.List.Split
import System.IO
import Utils
import BittorrentParser

import Data.Serialize
import Data.Serialize.Put
import Data.Serialize.Get
import Data.Either
--import  Data.ByteString.Lazy.Char8 as BSL
  
import  Data.ByteString.Char8 as BSL

data Packet = Packet ByteString ByteString
  deriving Show

getPacks :: ByteString -> [Packet]
getPacks = (P.map getPack) . (splitFor "5692433") -- because there's not enough magic in this world
  where
    getPack :: ByteString -> Packet
    getPack "" = Packet "" "" 
    getPack str = Packet header payload
      where
        header = BSL.takeWhile (/= '\n') str
        payload = BSL.tail . popLast $ BSL.drop (BSL.length header) str


getBTPack :: BSL.ByteString -> Either String Message
getBTPack = decode

main = do 
  let fName = "../data/packCap"
  contents <-BSL.readFile fName
  let packs =  P.map pieceSummary $ P.filter isPiece $ P.take 600 $ rights $ P.map (\(Packet _ p) -> getBTPack p) $ getPacks contents
  System.IO.putStrLn $ show $ packs


isPiece (Piece _ _ _) = True
isPiece _ = False

pieceSummary (Piece num i bs) = (num, i, BSL.length bs)