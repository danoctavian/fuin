{-# LANGUAGE OverloadedStrings #-}

import Crypto.Cipher.AES
import Data.ByteString.Char8 as DBC
import Data.Byteable
import Prelude as P
import Data.List as DL
import System.Random.MWC
import Control.Monad
import Control.Monad.Primitive
import System.IO
import Data.ByteString as DB
import Data.Word (Word8)

-- measure entropy of video file vs random file
c2w8 :: Char -> Word8
c2w8 = fromIntegral . fromEnum
charRangeStart :: Word8
charRangeStart = c2w8 '\0'
charRangeEnd :: Word8
charRangeEnd = c2w8 '\255'


textSize = 100000


makeRandomString = withSystemRandom $ \gen -> do
  genString gen textSize
  

genString :: Gen (PrimState IO) -> Int -> IO DB.ByteString
genString g len = do
    --randomLen <- uniformR (50 :: Int, 450 :: Int) g
    str <- replicateM len $ uniformR (charRangeStart, charRangeEnd) g
    return $ DB.pack str


-- this is probably incorrect - but it does kinda prove there is more redundancy in the video file...
compareEntropy = do
  randStr <- makeRandomString
  P.putStrLn $ "random string: "-- ++ (show randStr)
  P.putStrLn $show $ entropy $ show $ randStr
  video <- DBC.readFile "/home/dan/PORN/GullianaAlexisLollipop.mp4"
  P.putStrLn "video file:"
  P.putStrLn $ show $ entropy $ show $ DBC.drop textSize $ DBC.take (2 * textSize) video
  P.putStrLn "english text:"
  txtFile <- P.readFile "/home/dan/junk/poeCrit.txt"
  P.putStrLn $ show $ entropy $ P.take textSize txtFile
  return ()
 
entropy s = 
 DL.sum . DL.map lg' . fq' . DL.map (fromIntegral. DL.length) . DL.group . DL.sort $ s
  where lg' c = (c * ) . logBase 2 $ 1.0 / c
        fq' c = DL.map (\x -> x / (DL.sum c)) c 

aes = initAES $ DBC.pack $ P.take 32 $ P.repeat 'c' 


x =decryptECB aes $ DB.concat [(DBC.pack $ P.replicate 32 'x'), encryptECB aes sampleText]

sampleText = DBC.pack $ P.replicate 32 'a'
iv = DBC.pack $ P.replicate 32 'y'
doGCM = (enc, decryptGCM aes iv DBC.empty $ fst enc)
  where
    enc = encryptGCM aes iv DBC.empty sampleText
