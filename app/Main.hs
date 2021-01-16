module Main where

import PushPull

printToConsole :: Push String
printToConsole = Push putStrLn

writeToFile :: FilePath -> Push String
writeToFile fp = Push $ writeFile fp

readFromConsole :: Pull String
readFromConsole = Pull getLine

foo :: Push Int
foo = retain odd $ insert show $ forkN
  [ retain (\a -> length a > 9) $ writeToFile "/tmp/foo"
  , remove (\a -> length a > 9) $ printToConsole
  ]

foo1 :: Push Int
foo1 = retain odd $ insert show $ routeIf (\a -> length a > 9) (writeToFile "/tmp/foo") $ printToConsole

foo2 :: Push Int
foo2 = retain odd $ insert show $ forkIf (\a -> length a > 9) (writeToFile "/tmp/foo") $ printToConsole


main :: IO ()
main = do
  -- let foo = accept odd $ insert show printToConsole
  -- let foo = accept odd $ insert show $ writeToFile "/tmp/foo"
  push foo2 1000000001
  let bar = extract odd $ extract read readFromConsole
  f <- pull bar
  print f
  return ()
