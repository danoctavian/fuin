module Utils where
import Data.Word
import Data.Char
import Data.ByteString.Char8 as BSC
import Prelude as P

if' x a b = if x then a else b

fromRight (Right x) = x

toBool (Just x) = True
toBool Nothing = False 

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


