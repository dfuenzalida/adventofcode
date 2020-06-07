module Main where

import Day05
-- import Lib

mainFunc = do
  numLoops <- countLoops "resources/input05.txt" part2loop
  putStrLn $ show numLoops

main :: IO ()
main = mainFunc
