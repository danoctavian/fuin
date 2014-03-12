{-# LANGUAGE OverloadedStrings #-}
import Prelude as P
import Data.List.Split
import System.IO
import Utils
import WireProtocol

import Data.Serialize
import Data.Serialize.Put
import Data.Serialize.Get
import Data.Either
--import  Data.ByteString.Lazy.Char8 as BSL
  
import  Data.ByteString.Char8 as BSL

data Packet = Packet ByteString ByteString
  deriving Show

getPacks :: ByteString -> [Packet]
getPacks = (P.map getPack) . (splitFor "5692433")
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
  let fName = "packCap"
  contents <-BSL.readFile fName
  System.IO.putStrLn $ show $ P.take 20 $ rights $ P.map (\(Packet _ p) -> getBTPack p) $ getPacks contents
