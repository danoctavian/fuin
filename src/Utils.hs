module Utils where
import Data.Word
import Data.Char
import Data.ByteString.Char8 as BSC
import Prelude as P
import Data.ByteString.Internal (c2w)
import Data.ByteString.Lazy as DBSL
import Data.ByteString as DBS
import Data.Serialize
import Data.Serialize.Put
import Data.Serialize.Get
import Network.Socket
import Data.List.Split as DLS
import Data.List as DL

localhost = "localhost"

strToWord8s :: String -> [Word8]
strToWord8s = P.map c2w

if' x a b = if x then a else b
fromRight (Right x) = x
isRight (Right _) = True
isRight _ = False

select :: a -> [(Bool, a)] -> a
select def = maybe def snd . DL.find fst

maybeToBool (Just x) = True
maybeToBool Nothing = False 

word8ToChar :: Word8 -> Char
word8ToChar =  chr . fromIntegral

charsToInt :: Char -> Char -> Int
charsToInt a b = ord a * 2 ^ 8 + ord b

inbetween :: a -> [a] -> [a]
inbetween _ [x] =  [x]
inbetween i (x : xs) = x : i : (inbetween i xs)

toWord32 :: Word8 -> Word32
toWord32 = fromIntegral

powers x = P.map (x^) [0..]

-- normal bytestring utils
popLast bs = fst $ BSC.splitAt (BSC.length bs - 1) bs

splitFor :: BSC.ByteString -> BSC.ByteString -> [BSC.ByteString]
splitFor delim str = if' (rest == BSC.pack "") ([chunk])
                        (chunk : (splitFor delim $ BSC.drop dL rest))
  where
    (chunk, rest) = BSC.breakSubstring delim str
    dL = BSC.length delim

toStrict :: DBSL.ByteString -> DBS.ByteString
toStrict = DBS.concat . DBSL.toChunks

toggleEndianW16 :: Word16 -> Word16
toggleEndianW16 = fromRight . (runGet getWord16be) . runPut . putWord16le 

-- little endian portnumber; you get what you see basically
portNumberle :: Word16 -> PortNumber
portNumberle = PortNum . toggleEndianW16 

readIPv4 :: String -> Word32
readIPv4 s = word8sToWord32 $ P.map (\s -> read s :: Word8) $ DLS.splitOn "." s

-- little endian? fuck this shit idk it just works
word8sToWord32 :: [Word8] -> Word32
word8sToWord32 bytes =  sum $ P.zipWith (*)
                      ((P.take (P.length bytes)) $ powers (2 ^ 8)) 
                      (P.map toWord32 bytes)

iterateForever :: (Monad m) => (a -> m a) -> a -> m b
iterateForever f v = f v >>= iterateForever f


-- NETWORK UTILS


inferAddrFamily :: SockAddr -> Family 
inferAddrFamily (SockAddrInet _ _) = AF_INET
inferAddrFamily (SockAddrInet6 _ _ _ _) = AF_INET6
