module Day04 where

import System.IO
import qualified Data.Map as Map
import Data.Map (empty, filterWithKey, insertWith)

ex1a = words "aa bb cc dd ee"  -- valid
ex1b = words "aa bb cc dd aa"  -- NOT valid
ex1c = words "aa bb cc dd aaa" -- valid

frequencies xs = foldl (\m k -> insertWith (+) k 1 m) empty xs

valid1 xs = empty == (filterWithKey (\k v -> v > 1) $ frequencies xs)

part1 :: IO Int
part1 = do
  input <- readFile "resources/input04.txt"
  let contents = lines input
  return $ length $ filter (valid1 . words) contents

----------------------------------------------------------------------

ex2a = words "abcde fghij" -- valid
ex2b = words "abcde xyz ecdab" -- NOT valid
ex2c = words "a ab abc abd abf abj" -- valid
ex2d = words "iiii oiii ooii oooi oooo" -- valid
ex2e = words "oiii ioii iioi iiio" -- NOT valid

valid2 xs =
  empty == (filterWithKey (\k v -> v > 1) $ frequencies $ map frequencies xs)

-- [True,False,True,True,False] == map valid2 [ex2a, ex2b, ex2c, ex2d, ex2e]

part2 :: IO Int
part2 = do
  input <- readFile "resources/input04.txt"
  let contents = lines input
  return $ length $ filter (valid2 . words) contents
