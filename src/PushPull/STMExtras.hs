module PushPull.STMExtras where

import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar

modifyTMVar :: TMVar a -> (a -> a) -> STM ()
modifyTMVar var f = do
  v <- readTMVar var
  putTMVar var $ f v
