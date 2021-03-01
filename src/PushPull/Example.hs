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

data Person = Person {
  personId :: Int,
  personactualName :: String,
  personLastName :: String
}

isPersonValid :: Person -> Bool
isPersonValid p = personId p > 0

data Context = Context {
  contextCurrentPersonId :: Int,
  contextTime :: UTCTime
}

main :: IO ()
main = do
  -- data
  personIdVar <- newIORef 1
  actualNameVar <- newIORef "John"
  lastNameVar <- newIORef "Doe"
  let notification = putStrLn

  let
    -- cells
    aPersonId = cell "aPersonId" (writeIORef personIdVar) (readIORef personIdVar)
    aactualName = cell "aactualName" (writeIORef actualNameVar) (readIORef actualNameVar)
    aLastName = cell "aLastName" (writeIORef lastNameVar) (readIORef lastNameVar)
    -- pulls / entities
    quota = constant 100
    currentPersonId = contextCurrentPersonId <$> context
    currentTime = contextTime <$> context
    aPerson = Person <$> get aPersonId <*> get aactualName <*> get aLastName
    aPersonCaption = (\currentPersonId person quota -> (if personId person == currentPersonId then "Me" else personactualName person) <> show quota)  <$> currentPersonId <*> lifted aPerson <*> quota
    -- pushes / events
    timestampedNotification = enrich currentTime (\s t -> show t <> ": " <> show s) $ liftPush $ send notification
    personNameUpdate = fork (liftPush $ put aactualName) timestampedNotification
    validPersonNameUpdate = enrich (lifted aPerson) (,) $ routeIf (isPersonValid . snd) (map fst personNameUpdate) ignore
  t <- getCurrentTime
  -- pull aPersonCaption >>= print
  -- pushedRoots <- push validPersonNameUpdate (Context 1 t) "James"
  return ()
