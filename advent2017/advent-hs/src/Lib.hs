module Lib
    ( someFunc
    ) where

writeHello :: IO ()
writeHello = do
  putStrLn "hello!"

someFunc :: IO ()
someFunc = do
  writeHello
  putStrLn "hello"
  putStrLn "someFunc"
