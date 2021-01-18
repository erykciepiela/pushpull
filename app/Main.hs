module Main where

import PushPull
import Control.Concurrent
import Control.Monad
import Data.Time
import Control.Exception.Base

data MyContext = MyContext {
  currentTime :: UTCTime,
  currentUser :: String
}

getMyContext :: IO MyContext
getMyContext = MyContext <$> getCurrentTime <*> pure "anonymous user"

data MyException = MyException deriving (Show, Exception)

main :: IO ()
main = do
  printToConsole <- mkPush 100 putStrLn
  writeToFile <- mkPush 100 $ writeFile "/tmp/foo"
  let foo3 = validate (\s -> if length s > 4 then Right s else Left MyException) $ contextualize (\a c -> (a, currentTime c, currentUser c)) $ insert show $ printToConsole
  r <- push @MyException getMyContext foo3 "hello"
  print r
  threadDelay 1000000
