module Main where

import Prelude

import PushPull.Form
import Data.Time
import Data.Foldable (traverse_)

data Foo = Foo
  -- deriving Binary

main :: IO ()
main = do
  putStrLn "@"
  print $ fmap (+1) [0,1,2]
  interact undefined
  print $ foldl (+) 0 [0,1,2]
  _ <- traverse print [0,1,2]
  pure ()
