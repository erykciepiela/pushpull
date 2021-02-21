module PushPull.Example where

import Prelude hiding (map)

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue
import PushPull.Business
import Control.Monad
import Data.Time

import Data.IORef

data Person = Person {
  personId :: Int,
  personFirstName :: String,
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
  firstNameVar <- newIORef "John"
  lastNameVar <- newIORef "Doe"
  let notification = putStrLn

  let
    -- pulls / entities
    personId' = variable $ readIORef personIdVar
    firstName' = variable $ readIORef firstNameVar
    lastName' = variable $ readIORef lastNameVar
    quota' = constant 100
    currentPersonId' = contextCurrentPersonId <$> context
    currentTime' = contextTime <$> context
    person' = Person <$> personId' <*> firstName' <*> lastName'
    personCaption' = (\currentPersonId person quota -> (if personId person == currentPersonId then "Me" else personFirstName person) <> show quota)  <$> currentPersonId' <*> person' <*> quota'
    -- pushes / events
    timestampedNotification = enrich currentTime' (\s t -> show t <> ": " <> show s) $ change notification
    personNameUpdate = fork (change (writeIORef firstNameVar)) timestampedNotification
    validPersonNameUpdate = enrich person' (,) $ routeIf (isPersonValid . snd) (map fst personNameUpdate) ignore
  t <- getCurrentTime
  pull personCaption' (Context 1 t) >>= print
  push validPersonNameUpdate (Context 1 t) "James"
  return ()

-- main :: IO ()
-- main = do
--   -- tvars/tqueues
--   personIdVar <- newTVarIO 1
--   firstNameVar <- newTVarIO "John"
--   lastNameVar <- newTVarIO "Doe"
--   notification <- newTQueueIO

--   let
--     -- pulls / entities
--     personId' = variable $ readTVar personIdVar
--     firstName' = variable $ readTVar firstNameVar
--     lastName' = variable $ readTVar lastNameVar
--     quota' = constant 100
--     currentPersonId' = contextCurrentPersonId <$> context
--     currentTime' = contextTime <$> context
--     person' = Person <$> personId' <*> firstName' <*> lastName'
--     personCaption' = (\currentPersonId person quota -> (if personId person == currentPersonId then "Me" else personFirstName person) <> show quota)  <$> currentPersonId' <*> person' <*> quota'
--     -- pushes / events
--     timestampedNotification = enrich currentTime' (\s t -> show t <> ": " <> show s) $ change (writeTQueue notification)
--     personNameUpdate = fork (change (writeTVar firstNameVar)) timestampedNotification
--     validPersonNameUpdate = enrich person' (,) $ routeIf (isPersonValid . snd) (map fst personNameUpdate) ignore
--   return ()