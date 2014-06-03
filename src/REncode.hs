{-# LANGUAGE TupleSections #-}

module REncode where

import Prelude as P
import Data.ByteString.Lazy as DBL
import Data.Map as DM
import Data.Ix
import Data.Attoparsec as DA
import Data.Attoparsec.ByteString as DAB
import Data.Attoparsec.Char8 as DAC
import Data.Attoparsec.Binary
import Data.ByteString as DB
import Data.ByteString.Char8 as DBC
import Data.Serialize as DS
import Data.Serialize.Put
import Data.Serialize.Get
import Data.Serialize.IEEE754 as DSI
import Control.Applicative as CA
import Control.Monad

import Data.Int
import Data.BEncode
import Data.Char

import Data.Word

import Utils

data REncode = RInt Integer
       | RDouble Double
       | RFloat Float
       | RString DB.ByteString
       | RList [REncode]
       | RBool Bool
       | RDict (Map REncode REncode)
       | None
         deriving (Eq, Ord, Show)

instance Serialize REncode where
  get = undefined
  put = putREncoded

putREncoded (RInt x) = putWord8 chrInt8 *> (putWord64be (fromIntegral x))
putREncoded (RDouble x) = putWord8 chrFloat64 *> (putFloat64be x)
putREncoded (RFloat x) = putWord8 chrFloat32 *> (putFloat32be x)
putREncoded (RString bs)
  = if' (DB.length bs < fromIntegral strFixedCount)
      (putWord8 $ (fromIntegral $ DB.length bs) + strFixedStart)
      (putByteString (DBC.pack $ (show (DB.length bs)) ++ [colonTag]))
    *> putByteString bs
putREncoded (RList elems)
  = let contents = mapM putREncoded elems in 
      if' (P.length elems < fromIntegral listFixedCount)
      ((putWord8 $ listFixedStart + (fromIntegral $ P.length elems)) >> contents >> return ())
      (putWord8 chrList >> contents >> putWord8 chrTerm)
putREncoded (RDict dict)
  = let contents = mapM (\(k, v) -> putREncoded k *> putREncoded v) (DM.toList dict) in
      if' (DM.size dict < fromIntegral dictFixedCount)
      ((putWord8 $ dictFixedStart + (fromIntegral $ DM.size dict)) >> contents >> return ())
      (putWord8 chrDict >> contents >> putWord8 chrTerm)
putREncoded (RBool b) = putWord8 $ if' b chrTrue chrFalse
putREncoded None = putWord8 chrNone

rEncodeParser :: Parser REncode
rEncodeParser = rIntParser  <|> rFloatParser <|> rDoubleParser 
                <|> rBoolParser <|> rStringParser <|> rListParser <|> rDictParser

rStringParser :: Parser REncode 
rStringParser
  = (decimal <* (word8 . fromIntegral . ord $ colonTag))
    <|> 
    (fmap (fromIntegral . (\x -> x - strFixedStart) ) $
      DAB.satisfy (inRange $ byteRange strFixedStart strFixedCount))
    >>= (fmap RString) . DAB.take
      
rListParser = collParser (byteRange listFixedStart listFixedCount) rEncodeParser chrList RList
rDictParser = collParser (byteRange dictFixedStart dictFixedCount)
              ((, )  <$> rEncodeParser <*> rEncodeParser) chrDict (RDict . DM.fromList)

-- collection parser 
collParser sizeRange elemParser chr construct
  = fmap construct $
    (DAB.satisfy (inRange sizeRange)
      >>= \shiftedLen -> replicateM (fromIntegral $ shiftedLen - (P.fst sizeRange)) elemParser)
    <|> (word8 chr >> manyTill elemParser (word8 chrTerm))

--rIntParser = word8 chrInt8 >> DAB.take 8 >>= (return . RInt . fromIntegral . fromRight . (runGet getWord64be))

rIntParser = rIntegerParser chrInt8 8 getWord64be 
           <|> (rIntegerParser chrInt4 4 getWord32be)
           <|> (rIntegerParser chrInt1 1 getWord8)
           <|> (rIntegerParser chrInt2 2 getWord16be)
           <|> (RInt <$> (word8 chrInt *> DAC.signed decimal <* word8 chrTerm))
           <|> (RInt . fromIntegral . (\x -> x - intPosFixedStart)
                  <$> (DAB.satisfy (inRange $ byteRange intPosFixedStart intPosFixedCount)))
           <|> (RInt . negate . fromIntegral . (\x -> x - intNegFixedStart + 1)
                  <$> (DAB.satisfy (inRange $ byteRange intNegFixedStart intNegFixedCount)))

rIntegerParser chr  nBytes getter
  = (RInt . fromIntegral . fromRight . (runGet getter)) <$> ((word8 chr) *> (DAB.take nBytes))

rRationalParser construct getter chr nBytes
  = (construct . fromRight . (runGet getter)) <$> ((word8 chr) *> (DAB.take nBytes))
rFloatParser = rRationalParser RFloat getFloat32be chrFloat32 4
rDoubleParser = rRationalParser RDouble getFloat64be chrFloat64 8

rBoolParser =  (word8 chrTrue *> pure (RBool True)) <|> (word8 chrFalse *> pure (RBool False) )

byteRange start count = (start, start + count - 1)
fromRString (RString s) = s

testBenc = bPack $ BInt 32424253
testRenc = DB.length $ runPut $ putREncoded $ RList [RInt 9223372036854775807, RString (DBC.pack "mue"), RFloat 32]

repL n x = P.replicate n (RInt x)
simpleParse = parseOnly rEncodeParser $ runPut $ putREncoded $ RList $ (repL 40 2) P.++ [RList $ (repL 65 3)] P.++ (repL 25 4)

simpleStrParse = parseOnly rStringParser $ runPut $ putREncoded $ RString $ DBC.pack $ P.replicate 100 'w'
dictParse = parseOnly rDictParser $ runPut $ putREncoded $ RDict $ DM.fromList $ P.map (\x -> (RInt x, RInt x)) [1..50]


manyParseManualTest = do
  forM [0..14] $ \i -> do
    bs <- DB.readFile $ "../scripts/rencodeSamples/" P.++ (show i)
    P.putStrLn $ show $ parseOnly rEncodeParser bs
-- CONSTANTS
  -- Maximum length of integer when written as base 10 string.
maxIntLength = 64

  -- The bencode 'typecodes' such as i, d, etc have been extended and
  -- relocated on the base-256 character set.
chrList = 59
chrDict = 60
chrTerm = 127
chrInt = 61 -- arbitrary size
chrInt1 = 62
chrInt2 = 63
chrInt4 = 64
chrInt8 = 65
chrFloat32 = 66
chrFloat64 = 44
chrTrue = 67
chrFalse = 68
chrNone = 69

-- Strings with length embedded in typecode.
strFixedStart = 128
strFixedCount = 64
-- Lists with length embedded in typecode.
listFixedStart = strFixedStart + strFixedCount
listFixedCount = 64

-- Dictionaries with length embedded in typecode.
dictFixedStart = 102
dictFixedCount = 25

-- Positive integers with value embedded in typecode.
intPosFixedStart = 0
intPosFixedCount = 44
-- Negative integers with value embedded in typecode.
intNegFixedStart = 70
intNegFixedCount = 32

colonTag = ':'

