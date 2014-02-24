import Control.Concurrent.Chan
import Data.Char
import Control.Concurrent


readWords :: Chan String ->  IO ()
readWords chan = do
  line <- getLine
  writeChan chan line
  readWords chan


printWords :: Chan String -> IO ()
printWords chan =  do
  line <- readChan chan
  putStrLn $ map toUpper line
  printWords chan


main = do
  c <- newChan
  forkIO $ printWords c
  readWords c
