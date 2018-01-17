module Day04 where

import System.IO
import qualified Data.Map as Map
import Data.Map (empty, filterWithKey, insertWith)

ex1a = words "aa bb cc dd ee"  -- valid
ex1b = words "aa bb cc dd aa"  -- NOT valid
ex1c = words "aa bb cc dd aaa" -- valid

frequencies xs = foldl (\m k -> insertWith (+) k 1 m) empty xs
valid1 xs = empty == (filterWithKey (\k v -> v > 1) $ frequencies xs) 

mainloop :: Handle -> ([String] -> Bool) -> Integer -> IO ()
mainloop inputHandler f count =
  do isEOF <- hIsEOF inputHandler
     if isEOF
       then putStrLn $ "count = " ++ show(count)
       else do line <- hGetLine inputHandler
               let incr = if (f (words line)) then 1 else 0
               mainloop inputHandler f (count + incr)

part1 :: IO ()
part1 = do
  inputHandler <- openFile "resources/input04.txt" ReadMode
  mainloop inputHandler valid1 0
  hClose inputHandler

----------------------------------------------------------------------

ex2a = words "abcde fghij" -- valid
ex2b = words "abcde xyz ecdab" -- NOT valid
ex2c = words "a ab abc abd abf abj" -- valid
ex2d = words "iiii oiii ooii oooi oooo" -- valid
ex2e = words "oiii ioii iioi iiio" -- NOT valid

valid2 xs =
  empty == (filterWithKey (\k v -> v > 1) $ frequencies $ map frequencies xs)

-- [True,False,True,True,False] == map valid2 [ex2a, ex2b, ex2c, ex2d, ex2e]

part2 :: IO ()
part2 = do
  inputHandler <- openFile "resources/input04.txt" ReadMode
  mainloop inputHandler valid2 0
  hClose inputHandler
