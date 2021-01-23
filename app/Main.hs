module Main where

import PushPull
import PushPull.Runtime

import Control.Concurrent
import Control.Monad
import Data.Time
import Control.Exception.Base

data MyContext = MyContext {
  currentTime :: UTCTime,
  currentUser :: String
} deriving Show

data MyException = MyException deriving (Show, Exception)

main :: IO ()
main = do
  -- application I/O
  printToConsole <- pushOut 100 putStrLn
  printToFile <- pushOut 100 $ writeFile "/tmp/out"
  readFromFile <- pullIn 1000 $ readFile "/tmp/in"

  -- application business
  let pushWord = validate (\s -> if length s > 4 then Right s else Left MyException) $ enrich context (\a c -> (a, currentTime c, currentUser c)) $ insert show printToConsole
  let pullAge = (,) <$> extract length readFromFile <*> context

  -- rutime
  pushIn @MyException (MyContext <$> getCurrentTime <*> pure "alice") pushWord "hello"
  pullOut @MyException (MyContext <$> getCurrentTime <*> pure "bob") pullAge >>= print
  threadDelay 1000000
