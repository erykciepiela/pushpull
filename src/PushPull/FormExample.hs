module PushPull.FormExample where

import Prelude hiding (map)

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue
import PushPull.Business
import Control.Monad
import Data.Time

import Data.IORef
import Control.Monad.Trans.Except
import Data.Either.Combinators
import Text.Read hiding (get)

newtype PositiveInt = PositiveInt Int deriving Show

positiveInt :: Int -> Either String PositiveInt
positiveInt i
  | i > 0 = Right $ PositiveInt i
  | otherwise = Left "must be positive"

getPositiveInt :: PositiveInt -> Int
getPositiveInt (PositiveInt i) = i

newtype NonEmptyString = NonEmptyString String deriving Show

nonEmptyString :: String -> Either String NonEmptyString
nonEmptyString s
  | null s = Left "must be non empty"
  | otherwise = Right $ NonEmptyString s

getNonEmptyString :: NonEmptyString -> String
getNonEmptyString (NonEmptyString s) = s

newtype Month = Month Int deriving Show

month :: Int -> Maybe Month
month i
  | i > 0 && i < 13 = Just $ Month i
  | otherwise = Nothing

data Person = Person {
  age :: PositiveInt,
  name :: NonEmptyString,
  note :: String
} deriving Show

type Log = String

mkPerson :: PositiveInt -> NonEmptyString -> String -> Either String (Person, String)
mkPerson age name note = if getPositiveInt age < 10 && length (getNonEmptyString name) > 10 then Left "Improper name for a child" else Right (Person age name note, "person 1")

main :: IO ()
main = do
  -- data
  ageVar <- newIORef 0
  nameVar <- newIORef ""
  let notification = putStrLn

  let
    ageCell = cell "age" (writeIORef ageVar) (readIORef ageVar)
    nameCell = cell "name" (writeIORef nameVar) (readIORef nameVar)
    noteCell = cell "note" (writeIORef nameVar) (readIORef nameVar)
    monthCell = cell "month" (writeIORef nameVar) (readIORef nameVar)
    age = right $ positiveInt <$> get ageCell
    ageError = left $ positiveInt <$> get ageCell
    name = right $ nonEmptyString <$> get nameCell
    nameError = left $ nonEmptyString <$> get nameCell
    person = actual $ right $ mkPerson <$> age <*> name <*> lifted (get noteCell)
    personError = left $ mkPerson <$> age <*> name <*> lifted (get noteCell)
    personLog = second $ right $ mkPerson <$> age <*> name <*> lifted (get noteCell)
    foo = undefined <$> person <*> lifted (lifted age)
    a = existing $ month <$> existing (readMaybe @Int <$> get monthCell)
    -- pushes / events
  pull (unright (unright (unactual person))) () >>= print
  push (put ageCell) () 1
  pull (unright (unright (unactual person))) () >>= print
  push (put nameCell) () "Konstantynopolitanka"
  pull (unright (unright (unactual person))) () >>= print
  push (put nameCell) () "Kostek"
  pull (unright (unright (unactual person))) () >>= print
  -- pushedRoots <- push validPersonNameUpdate (Context 1 t) "James"
  -- print pushedRoots
  -- pulledRoots <- pullRoots aPersonCaption (Context 1 t)
  -- print pulledRoots
  return ()
