module Main where

import Prelude hiding (map)

import PushPull
import PushPull.Runtime

import Control.Concurrent
import Data.Time
import Control.Exception.Base

data MyContext = MyContext {
  currentTime :: UTCTime,
  currentUser :: String
} deriving Show

data MyException = MyException deriving (Show, Exception)

type Application i o = i -> o

app :: Application (Push MyContext String, Push MyContext String, Pull MyContext String) (Push MyContext String, Pull MyContext (Int, MyContext))
app (printToConsole, printToFile, readFromFile) = let
  pushWord = validate (\s -> if length s > 4 then Right s else Left MyException) $ enrich context (\a c -> (a, currentTime c, currentUser c)) $ map show printToConsole
  pullAge = (,) <$> (length <$> readFromFile) <*> context
  in (pushWord, pullAge)

main :: IO ()
main = do
  -- application I/O
  printToConsole <- pushOut 100 putStrLn
  printToFile <- pushOut 100 $ writeFile "/tmp/out"
  readFromFile <- pullIn 1000 $ readFile "/tmp/in"

  -- application business
  let (pushWord, pullAge) = app (printToConsole, printToFile, readFromFile)

  -- rutime
  pushIn @MyException (MyContext <$> getCurrentTime <*> pure "alice") pushWord "hello"
  pullOut @MyException (MyContext <$> getCurrentTime <*> pure "bob") pullAge >>= print
  threadDelay 1000000
