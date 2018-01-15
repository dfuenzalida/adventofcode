module Day03 where

import Data.Map

shifts = fromList([("left", (-1,0)), ("right",(1,  0)),
                   ("up",   ( 0,1)), ("down", (0, -1))])

moves :: Int -> [[Char]]
moves n = firstPart ++ secondPart where
  firstPart = Prelude.take n $ repeat $ if (odd n) then "right" else "left"
  secondPart = Prelude.take n $ repeat $ if (odd n) then "up" else "down"

path :: Int -> [[Char]]
path n = Prelude.take (n-1) allMoves where
  allMoves = concatMap moves [0..]

distance :: Int -> Int
distance n = firsts + seconds where
  firsts = abs $ foldl1 (+) $ Prelude.map fst steps
  seconds = abs $ foldl1 (+) $ Prelude.map snd steps
  steps = Prelude.map (shifts !) $ path n

-- *Main Day03 Data.Map> distance 12
-- 3
-- *Main Day03 Data.Map> distance 23
-- 2
-- *Main Day03 Data.Map> distance 1024
-- 31

----------------------------------------------------------------------

allMoves = concatMap moves [1..]
sumPairs (a,b) (c,d) = (a+c, b+d)

computeGrid grid (x,y) = foldl1 (+) neighborVals where
  neighborVals = Prelude.map (\p -> findWithDefault 0 p grid) $ neighbors (x,y)
  neighbors (x,y) = zipWith sumPairs allDeltas (repeat (x,y))
  allDeltas = Prelude.filter (\x -> x /= (0,0)) $ allPairs [-1, 0, 1] [-1, 0, 1]
  allPairs xs ys = concatMap (\x->(Prelude.map (\y->(x, y))) ys) xs

fillGrid grid (x,y) movesList limit =
  if curr > limit then curr else fillGrid newGrid newCoord newMoves limit where
    curr = computeGrid grid (x,y)
    newGrid = insert (x,y) curr grid
    newCoord = sumPairs (x,y) $ shifts ! (head movesList)
    newMoves = tail movesList

-- *Main Day03 Data.Map> fillGrid (fromList [((0,0), 1)]) (1,0) (Prelude.drop 1 allMoves) 80
-- 122
-- *Main Day03 Data.Map> fillGrid (fromList [((0,0), 1)]) (1,0) (Prelude.drop 1 allMoves) 999999
-- 1009457

