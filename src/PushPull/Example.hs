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
    -- cells
    personId' = cell (writeIORef personIdVar) (readIORef personIdVar)
    firstName' = cell (writeIORef firstNameVar) (readIORef firstNameVar)
    lastName' = cell (writeIORef lastNameVar) (readIORef lastNameVar)
    -- pulls / entities
    quota' = constant 100
    currentPersonId' = contextCurrentPersonId <$> context
    currentTime' = contextTime <$> context
    person' = Person <$> get personId' <*> get firstName' <*> get lastName'
    personCaption' = (\currentPersonId person quota -> (if personId person == currentPersonId then "Me" else personFirstName person) <> show quota)  <$> currentPersonId' <*> person' <*> quota'
    -- pushes / events
    timestampedNotification = enrich currentTime' (\s t -> show t <> ": " <> show s) $ send notification
    personNameUpdate = fork (put firstName') timestampedNotification
    validPersonNameUpdate = enrich person' (,) $ routeIf (isPersonValid . snd) (map fst personNameUpdate) ignore
  t <- getCurrentTime
  pull personCaption' (Context 1 t) >>= print
  push validPersonNameUpdate (Context 1 t) "James"
  return ()
