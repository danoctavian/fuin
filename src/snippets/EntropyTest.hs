{-# LANGUAGE OverloadedStrings #-}

import Crypto.Cipher.AES
import Data.ByteString.Char8 as DBC
import Data.Byteable
import Prelude as P
import Data.List as DL
import System.Random.MWC
import Control.Monad as CM
import Control.Monad.Primitive
import System.IO
import Data.ByteString as DB
import Data.Word (Word8)
import Data.Char
import Data.Conduit
--import Data.Vector.Mutable as DVM
import Data.Conduit.Binary as DCB
import Data.Vector.Unboxed as DV
import Data.Conduit.List as DCL
import System.IO
import Data.Traversable as DT
import Control.Monad.IO.Class
import Prelude as P
import Data.Vector.Unboxed.Mutable as DVM
import System.Directory
import  Data.ByteString.Lazy.Char8 as BSL
import  Data.ByteString.Char8 as BSC
import Control.Monad.Trans.Class

import System.Entropy
-- measure entropy of video file vs random file
c2w8 :: Char -> Word8
c2w8 = fromIntegral . fromEnum
charRangeStart :: Word8
charRangeStart = c2w8 '\0'
charRangeEnd :: Word8
charRangeEnd = c2w8 '\255'


textSize = 1000000

makeRandomString = withSystemRandom $ \gen -> do
  genString gen textSize
  

genString :: Gen (PrimState IO) -> Int -> IO DB.ByteString
genString g len = do
    --randomLen <- uniformR (50 :: Int, 450 :: Int) g
    str <- CM.replicateM len $ uniformR (0, 255) g
    return $ DB.pack str


-- inefficient entropy implementation
entropy s = 
 DL.sum . DL.map lg . fq . DL.map (fromIntegral. DL.length) . DL.group . DL.sort $ s

lg c = (c * ) . logBase 2 $ 1.0 / c
fq c = DL.map (\x -> x / (DL.sum c)) c 

histoToEntropy  = DL.sum . DL.map lg . fq



entropySink :: (PrimMonad m, MonadIO m) => Sink DB.ByteString m Double        
entropySink = do
  histo <- lift $ DVM.replicate 256 (0 :: Double)
  loopEntropy histo

loopEntropy :: (PrimMonad m, MonadIO m) => DVM.MVector (PrimState m) Double -> Sink DB.ByteString m Double
loopEntropy histo = do
  DCL.mapM_ $ \bs -> do
    CM.forM  (DB.unpack bs) $ \b ->
      Main.modify histo b (+1)
    return ()
  frozen <- lift $ DV.freeze histo
  return $ histoToEntropy $  DV.toList frozen
    
modify v i f = DVM.unsafeRead v (fromIntegral i) >>= (DVM.unsafeWrite v (fromIntegral i) . f)

-- this is probably incorrect - but it does kinda prove there is more redundancy in the video file...
compareEntropy = do
  randBytes <- getEntropy textSize
  randStr <- makeRandomString
  entr <- DCL.sourceList [randStr] $$ entropySink
  P.putStrLn $ "random string: "-- ++ (show randStr)
  P.putStrLn $show $ entropy $ DB.unpack randBytes
  P.putStrLn $show $ entr
  let pornPath = "/home/dan/PORN/GullianaAlexisLollipop.mp4"
 -- video <- DBC.readFile "/home/dan/PORN/GullianaAlexisLollipop.mp4"
  P.putStrLn "video file:"
  pfile <- openFile pornPath ReadMode
  --let select = DBC.take (10000000) . DBC.drop (1000000 * 2)
  vidEntropy <- sourceHandle pfile $$ entropySink
  P.putStrLn $ show $ vidEntropy
 -- P.putStrLn $ show $ entropy $ DB.unpack $ select video
  P.putStrLn "english text:"
  txtFile <- P.readFile "/home/dan/junk/poeCrit.txt"
  P.putStrLn $ show $ entropy $ P.filter isLetter $ P.take textSize txtFile
  return ()
 
randEntropyMeasurement = do
  CM.forM [1..10] $ \i -> do 
    randBytes <- getEntropy textSize
    e <- DCL.sourceList [randBytes] $$ entropySink
    P.putStrLn $ show e

runEntropyOnVids = do
  let pornPath = "/home/dan/PORN/harcor/" 
  --vids <- getDirectoryContents pornPath1
  vids <- return ["Jenna Justine juts loves huge black worms.mp4", "18825_720x406_1000k.mp4", "Suits.S03E01.HDTV.x264-EVOLVE 2.mp4"]
  CM.forM vids $ \vid ->
    computeFileEntropy (pornPath P.++ vid)

computeFileEntropy filePath = do
  P.putStrLn $ "File " P.++ filePath P.++ "  computing now... "
  pfile <- openFile filePath ReadMode
  vidEntropy <- sourceHandle pfile $$ entropySink
  P.putStrLn $ "File " P.++ filePath P.++ " has entropy " P.++ (show vidEntropy)

main = do
  measureBTEntropy

aes = initAES $ DBC.pack $ P.take 32 $ P.repeat 'c' 



x =decryptECB aes $ DB.concat [(DBC.pack $ P.replicate 32 'x'), encryptECB aes sampleText]

sampleText = DBC.pack $ P.replicate 32 'a'
iv = DBC.pack $ P.replicate 32 'y'
doGCM = (enc, decryptGCM aes iv DBC.empty $ fst enc)
  where
    enc = encryptGCM aes iv DBC.empty sampleText


data Packet = Packet DB.ByteString DB.ByteString

popLast bs = fst $ BSC.splitAt (BSC.length bs - 1) bs


splitFor :: BSC.ByteString -> BSC.ByteString -> [BSC.ByteString]
splitFor delim str = if (rest == BSC.pack "")then ([chunk])
                        else (chunk : (splitFor delim $ BSC.drop dL rest))
  where
    (chunk, rest) = BSC.breakSubstring delim str
    dL = BSC.length delim


getPacks :: DB.ByteString -> [Packet]
getPacks = (P.map getPack) . (splitFor "5692433") -- because there's not enough magic in this world
  where
    getPack :: DB.ByteString -> Packet
    getPack "" = Packet "" "" 
    getPack str = Packet header payload
      where
        header = BSC.takeWhile (/= '\n') str
        payload = BSC.tail . popLast $ BSC.drop (DB.length header) str    

measureBTEntropy = do
  let root = "../../scripts/"
  let fName = "packCap2014-06-12|18:21:16"
  let fnName2 = "packCap2014-06-12|20:08:20"
  for packCapFiles $ \name -> do
    contents <-DB.readFile (root P.++ name)
    let packs =  P.map (\(Packet _ p) -> p) $ getPacks contents
    e <- DCL.sourceList packs $$ entropySink
    System.IO.putStrLn $ show $ e
{-
c(,7.99157629972148,7.997941565128213,7.966336873086672,7.9972912459707395,7.975495234485001,7.997518193007396,7.997384338583383,7.998733995232448,7.9972912459707395,7.9872912459707395,7.9956912459707395)
-}
{-
c(7.999825214213944,7.999829250269812,7.999829404213091,7.999801312845717,7.999800303705827,7.999827357773468,7.999813628831798,7.999803080990293,7.999776735043576,7.9998380952923105)
-}

packCapFiles = ["packCap2014-06-12|18:21:16","packCap2014-06-12|20:08:20","packCap2014-06-12|20:09:37","packCap2014-06-12|20:10:39","packCap2014-06-12|20:10:41","packCap2014-06-12|20:12:33"]

