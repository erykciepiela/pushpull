module PushPull.Example where

import Prelude hiding (map)

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue
import PushPull.Business
import Control.Monad
import Data.Time

import Data.IORef
import Control.Monad.Trans.Reader
import PushPull.Model

printToConsole :: Push IO String
printToConsole = Push putStrLn
