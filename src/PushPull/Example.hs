module PushPull.Example where

import Prelude hiding (map)

import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue
import PushPull.Business
import Control.Monad
import Data.Time

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
  -- tvars/tqueues
  personIdVar <- newTVarIO 1
  firstNameVar <- newTVarIO "John"
  lastNameVar <- newTVarIO "Doe"
  updateNotification <- newTQueueIO

  let
    -- pulls
    personId' = variable personIdVar
    firstName' = variable firstNameVar
    lastName' = variable lastNameVar
    quota' = constant 100
    currentPersonId' = contextCurrentPersonId <$> context
    currentTime' = contextTime <$> context
    person' = Person <$> personId' <*> firstName' <*> lastName'
    personCaption' = (\currentPersonId person -> if personId person == currentPersonId then "Me" else personFirstName person) <$> currentPersonId' <*> person'
    -- pushes
    -- sendTimestampedNotification :: Show a => Push Context a
    sendTimestampedNotification = enrich currentTime' (\s t -> show t <> ": " <> show s) $ send updateNotification
    updatePersonName = fork (change firstNameVar) sendTimestampedNotification
    updateValidPersonName = enrich person' (,) $ routeIf (isPersonValid . snd) (map fst updatePersonName) ignore
    -- or: (?)
    -- let updateValidPersonName' = \newName -> do
    --       p <- readUpdate person'
    --       when (isPersonValid p) $ do
    --         writeUpdate firstNameVar newName
  return ()