module Main where

import PushPull

printToConsole :: Push String
printToConsole = Push putStrLn

readFromConsole :: Pull String
readFromConsole = Pull getLine

main :: IO ()
main = do
  let foo = accept odd $ insert show printToConsole
  push foo 1
  let bar = extract odd $ extract read readFromConsole
  f <- pull bar
  print f
  return ()
