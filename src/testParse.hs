{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Char8
import Data.Word
import Control.Applicative

-- | Type for IP's.
data IP = IP Word8 Word8 Word8 Word8 deriving Show
data Nums = Nums Word8 Word8 deriving Show



parseIP :: Parser IP
parseIP = do
  d1 <- decimal
  char '.'
  d2 <- decimal
  char '.'
  d3 <- decimal
  char '.'
  d4 <- decimal
  return $ IP d1 d2 d3 d4

parseNums :: Parser Nums
parseNums = (pure Nums) <*> decimal <* (char '.') <*> decimal
main :: IO ()
main = do
  putStrLn "wtf"
  print $ parseOnly parseNums "131.2"
  print $ parseOnly parseIP "131.45.68.123"
