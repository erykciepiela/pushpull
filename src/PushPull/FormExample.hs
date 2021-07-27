module PushPull.FormExample where

import Prelude hiding (map, either)

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue
import PushPull.Business
import Control.Monad
import Data.Time
import Data.IORef
import Control.Monad.Trans.Except
import Data.Either.Combinators
import Text.Read hiding (get, lift)
import Data.Maybe
import PushPull.Form
import Data.Monoid
import Control.Monad.Trans

newtype PositiveInt = PositiveInt Int deriving Show

positiveIntOrError :: Int -> Either String PositiveInt
positiveIntOrError i
  | i > 0 = Right $ PositiveInt i
  | otherwise = Left "must be positive"

getPositiveInt :: PositiveInt -> Int
getPositiveInt (PositiveInt i) = i

newtype NonEmptyString = NonEmptyString String deriving Show

nonEmptyStringOrError :: String -> Either String NonEmptyString
nonEmptyStringOrError s
  | null s = Left "must be non empty"
  | otherwise = Right $ NonEmptyString s

getNonEmptyString :: NonEmptyString -> String
getNonEmptyString (NonEmptyString s) = s

data Title = Mr | Ms deriving Show

data ShirtSize = S | M | L | XL  deriving Show

data Person = Person {
  personAge :: PositiveInt,
  personName :: NonEmptyString,
  personTitle :: Title,
  personShirtSize :: ShirtSize
} deriving Show

validPerson :: PositiveInt -> NonEmptyString -> Title -> ShirtSize -> Either String Person
validPerson age name title shirtSize = if getPositiveInt age < 10 && length (getNonEmptyString name) > 10 then Left "Improper name for a child" else Right (Person age name title shirtSize)

printConsole :: Show s => Push IO s
printConsole = send print

data Choices = Choices
  { shirtSizes :: [ShirtSize]
  , titles :: [Title]
  }

main :: IO ()
main = do
  ageField <- newFormField
  nameField <- newFormField
  titleField <- newFormField
  shirtSizeField <- newFormField

  let
    ageOrError = positiveIntOrError <$> formFieldValue ageField
    age = justOf $ rightToMaybe <$> ageOrError
    ageError = justOf $ leftToMaybe <$> ageOrError
    nameOrError = nonEmptyStringOrError <$> formFieldValue nameField
    name = justOf $ rightToMaybe <$> nameOrError
    nameError = justOf $ leftToMaybe <$> nameOrError
    personOrError = validPerson <$> age <*> name <*> formFieldValue titleField <*> formFieldValue shirtSizeField
    person = justOf $ rightToMaybe <$> personOrError
    personError = justOf $ leftToMaybe <$> personOrError

    printForm = displayDynamic (Choices [S, M, L, XL] [Mr, Ms]) $ do
      lift $ putStrLn "-----"
      -- putStr "Age: "
      -- readFormField ageField >>= print
      whenForm ageError (== Touched) $ \a -> putStrLn ("  invalid age: " <> a)

      -- putStr "Name: "
      -- readFormField nameField >>= print
      whenForm nameError (== Touched) $ \a -> putStrLn ("  invalid name: " <> a)

      -- putStr "Note: "
      -- readFormField noteField >>= print
      whenForm person (== Touching) $ \a -> putStrLn "person: ..."
      whenForm person (== Touched) $ \a -> putStrLn ("person: " <> show a)

      whenForm personError (== Touched) $ \a -> putStrLn ("  invalid person: " <> a)

  writeFormField ageField $ const $ Just (0, NotTouched)
  writeFormField nameField $ const $ Just ("", NotTouched)
  writeFormField titleField $ \(Choices _ titles) -> Just (titles !! 0, NotTouched)
  writeFormField shirtSizeField $ \(Choices sizes _) -> Just (sizes !! 0, NotTouched)
  printForm

  writeFormField ageField $ const $ Just (0, Touched)
  printForm
  writeFormField ageField $ const $ Just (5, Touched)
  printForm
  writeFormField nameField $ const $ Just ("K", Touching)
  printForm
  writeFormField nameField $ const $ Just ("", Touched)
  printForm
  writeFormField nameField $ const $ Just ("K", Touching)
  printForm
  writeFormField nameField $ const $ Just ("Konstantynopolitanka", Touched)
  printForm
  writeFormField nameField $ const $ Just ("Kostek", Touched)
  printForm
  writeFormField shirtSizeField $ \(Choices sizes _) -> Just (sizes !! 2, Touched)
  printForm
  writeFormField titleField $ \(Choices _ titles) -> Just (titles !! 1, Touched)
  printForm


  return ()
