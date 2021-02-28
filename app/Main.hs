module Main where

import Prelude hiding (map, read)

import PushPull.Business
import Control.Concurrent
import Data.Time
import Control.Exception.Base
import qualified PushPull.Example as E
import qualified PushPull.FormExample as FE

data MyContext = MyContext {
  currentTime :: UTCTime,
  currentUser :: String
} deriving Show

data MyException = MyException deriving (Show, Exception)

type Business i o = i -> o

-- business :: Business (Push MyContext String, Push MyContext String, Pull MyContext String) (Push MyContext String, Pull MyContext (Int, MyContext))
-- business (printToConsole, printToFile, readFromFile) = let
--   pushWord = validate (\s -> if length s > 4 then Right s else Left MyException) $ enrich context (\a c -> (a, currentTime c, currentUser c)) $ map show printToConsole
--   pullAge = (,) <$> (length <$> readFromFile) <*> context
--   in (pushWord, pullAge)

-- main' :: IO ()
-- main' = do
--   -- I/O
--   printToConsole <- pushOut 100 putStrLn
--   printToFile <- pushOut 100 $ writeFile "/tmp/out"
--   readFromFile <- pullIn 1000 $ readFile "/tmp/in"

--   -- business
--   let (pushWord, pullAge) = business (printToConsole, printToFile, readFromFile)

--   -- rutime
--   pushIn @MyException (MyContext <$> getCurrentTime <*> pure "alice") pushWord "hello"
--   pullOut @MyException (MyContext <$> getCurrentTime <*> pure "bob") pullAge >>= print
--   threadDelay 1000000

main :: IO ()
main = do
  -- nameCell <- latest
  -- addressCell <- latest
  -- let clear = replace "" $ write nameCell <> write addressCell
  -- let name = read nameCell
  -- let address = read addressCell
  FE.main