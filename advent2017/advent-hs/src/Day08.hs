module Day08 where

import Data.Map as Map hiding (null, foldl, map)

example = ["b inc 5 if a > 1",
            "a inc 1 if b < 5",
            "c dec -10 if a >= 1",
            "c inc -20 if c == 10"]

--                         "b"    "inc"  5       "a"    ">"    1
data Operation = Operation String String Integer String String Integer
  deriving (Eq, Show)

parseLine line = Operation reg op amtInt reg2 cmp cmpValInt where
  (reg:op:amt:_:reg2:cmp:cmpVal:xs) = words line
  amtInt = read amt :: Integer
  cmpValInt = read cmpVal :: Integer

mutOps :: Map String (Integer -> Integer -> Integer)
mutOps = fromList [("inc", (+)), ("dec", (-))]

cmpOps :: Map String (Integer -> Integer -> Bool)
cmpOps = fromList [("<", (<)), ("<=", (<=)), ("==", (==)),
                   (">", (>)), (">=", (>=)), ("!=", (/=))]

processOp :: Map String Integer -> Operation -> Map String Integer
processOp m (Operation reg op amtInt reg2 cmp cmpValInt) = let
  u = (findWithDefault (==) cmp cmpOps) (findWithDefault 0 reg2 m) cmpValInt
  oldVal = findWithDefault 0 reg m
  newVal = (findWithDefault (+) op mutOps) oldVal amtInt
  in if u then (insert reg newVal m) else m

-- maximum $ elems $ foldl processOp empty $ map parseLine example

part1 = do
    fileContents <- readFile "resources/input08.txt"
    let fileLines = lines fileContents
    let result = foldl processOp empty $ map parseLine fileLines
    putStrLn $ show $ maximum $ elems $ result

----------------------------------------------------------------------

processOp' :: (Map String Integer, Integer) -> Operation -> (Map String Integer, Integer)
processOp' (m,mv) (Operation reg op amtInt reg2 cmp cmpValInt) = let
  u = (findWithDefault (==) cmp cmpOps) (findWithDefault 0 reg2 m) cmpValInt
  oldVal = findWithDefault 0 reg m
  newVal = (findWithDefault (+) op mutOps) oldVal amtInt
  mv' = max mv newVal
  in if u then ((insert reg newVal m), mv') else (m,mv)

part2 = do
    fileContents <- readFile "resources/input08.txt"
    let fileLines = lines fileContents
    let result = foldl processOp' (empty,0) $ map parseLine fileLines
    putStrLn $ show $ snd $ result
