module Main where

import PushPull
import Control.Concurrent
import Control.Monad
import Data.Time

printToConsole :: Push String
printToConsole = Push $ const putStrLn

writeToFile :: FilePath -> Push String
writeToFile fp = Push $ const $ writeFile fp

readFromConsole :: Pull String
readFromConsole = Pull getLine

currentTime :: Pull UTCTime
currentTime = Pull getCurrentTime

foo :: Push Int
foo = retain odd $ insert show $ forkN
  [ retain (\a -> length a > 9) $ writeToFile "/tmp/foo"
  , remove (\a -> length a > 9) $ printToConsole
  ]

foo1 :: Push Int
foo1 = retain odd $ insert show $ routeIf (\a -> length a > 9) (writeToFile "/tmp/foo") $ printToConsole

foo2 :: Push Int
foo2 = retain odd $ insert show $ forkIf (\a -> length a > 9) (writeToFile "/tmp/foo") $ printToConsole

foo3 :: Push String
foo3 = contextualize (\a c -> (a, pushTime c)) $ insert show $ printToConsole

main :: IO ()
main = do
  push foo3 "hello"
  -- cell <- PushPull.all
  -- forkIO $ forever $ do
  --   v <- pull (readCell cell)
  --   print v
  --   threadDelay 1000000
  -- forever $ do
  --   l <- getLine
  --   push (writeCell cell) l
