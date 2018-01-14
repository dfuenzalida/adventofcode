module Day03 where

import Data.Map

data Direction = LeftDir | RightDir | UpDir | DownDir
               deriving (Eq, Ord, Show)

shifts = fromList([(LeftDir, (-1,0)), (RightDir,(1,  0)),
                   (UpDir,   ( 0,1)), (DownDir, (0, -1))])

moves n = firstPart ++ secondPart where
  firstPart = Prelude.take n $ repeat $ if (odd n) then RightDir else LeftDir
  secondPart = Prelude.take n $ repeat $ if (odd n) then UpDir else DownDir

path n = Prelude.take (n-1) allMoves where
  allMoves = concatMap moves [0..]

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
