module PushPull.Runtime where

import PushPull

import Control.Exception.Base
import Control.Monad.STM
import Control.Concurrent.STM.TBQueue
import Control.Concurrent
import Control.Monad
import Control.Concurrent.STM.TMVar

-- I/O

pushOut :: Int -> (a -> IO ()) -> IO (Push ctx a)
pushOut queueSize consume = do
  q <- newTBQueueIO (fromIntegral queueSize)
  forkIO $ forever $ do
    a <- atomically $ readTBQueue q
    consume a
  return $ Push $ const $ writeTBQueue q

pullIn :: Int -> IO a -> IO (Pull ctx a)
pullIn periodMilliseconds producer = do
  a <- producer
  var <- newTMVarIO a
  forkIO $ forever $ do
    threadDelay $ periodMilliseconds * 1000
    a <- producer
    atomically $ putTMVar var a
  return $ Pull $ const $ readTMVar var


pushIn :: Exception e => IO ctx -> Push ctx a -> a -> IO (Either e ())
pushIn getContext (Push p) a = do
  c <- getContext
  atomically $ catchSTM (Right <$> p c a) (return . Left)

pullOut :: Exception e => IO ctx -> Pull ctx a -> IO (Either e a)
pullOut getContext (Pull p) = do
  c <- getContext
  atomically $ catchSTM  (Right <$> p c) (return . Left)
