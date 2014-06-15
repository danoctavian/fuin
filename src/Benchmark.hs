
{-# LANGUAGE ScopedTypeVariables #-}

import Criterion.Measurement
import Prelude as P
import Control.Applicative
import Control.Concurrent
import Criterion.Main
import Control.Parallel
import qualified Data.IntMap as I
import Data.List (foldl')
import Criterion.Config
import Network.HTTP.Conduit

del = threadDelay $ 10 ^ 5

httpBench = defaultMainWith defaultConfig (return ()) [
         bgroup "nginxHTTPFetch" $ P.map makeHTTPFetchBench [1..10]
       ]

makeHTTPFetchBench size
  = bench ("http " ++ (show size)) $ whnfIO
    ((simpleHttp $ "http://185.53.129.145/" ++ "file" ++ (show size) ++ "mb")
      >> return ())


main = do
  httpBench