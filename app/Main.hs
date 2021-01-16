module Main where

import PushPull

printToConsole :: Push String
printToConsole = Push putStrLn

readFromConsole :: Pull String
readFromConsole = Pull getLine

main :: IO ()
main = do
  push (odd `insert` show `insert` printToConsole) 1
  f <- pull (readFromConsole `extract` read `extract` odd)
  return ()
