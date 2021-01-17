module Main where

import PushPull
import Control.Concurrent
import Control.Monad
import Data.Time
import Control.Exception.Base

-- foo :: Push Int
-- foo = retain odd $ insert show $ forkN
--   [ retain (\a -> length a > 9) $ writeToFile "/tmp/foo"
--   , remove (\a -> length a > 9) $ printToConsole
--   ]

-- foo1 :: Push Int
-- foo1 = retain odd $ insert show $ routeIf (\a -> length a > 9) (writeToFile "/tmp/foo") $ printToConsole

-- foo2 :: Push Int
-- foo2 = retain odd $ insert show $ forkIf (\a -> length a > 9) (writeToFile "/tmp/foo") $ printToConsole

-- foo3 :: Push String
-- foo3 = contextualize (\a c -> (a, pushTime c)) $ insert show $ printToConsole

data PushPullContext = PushPullContext {
  currentTime :: UTCTime,
  currentUser :: String
}

getPPContext :: IO PushPullContext
getPPContext = PushPullContext <$> getCurrentTime <*> pure "anonymous user"

data MyException = ThisException | ThatException deriving Show

instance Exception MyException

main :: IO ()
main = do
  printToConsole <- mkPush putStrLn
  writeToFile <- mkPush $ writeFile "/tmp/foo"
  let foo3 = validate (\s -> if length s > 4 then Right s else Left ThisException) $ contextualize (\a c -> (a, currentTime c, currentUser c)) $ insert show $ printToConsole
  r <- push @MyException getPPContext foo3 "hello"
  print r
  threadDelay 1000000
  -- cell <- PushPull.all
  -- forkIO $ forever $ do
  --   v <- pull (readCell cell)
  --   print v
  --   threadDelay 1000000
  -- forever $ do
  --   l <- getLine
  --   push (writeCell cell) l
