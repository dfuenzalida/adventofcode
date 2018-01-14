module Day02 where

spreadsheet1 = [[5,1,9,5],
                [7,5,3],
                [2,4,6,8]]

part1 :: (Ord a, Num a) => [[a]] -> a
part1 xss = foldl1 (+) diffs where
  diffs = zipWith (-) maxs mins
  maxs = map (foldl1 max) xss
  mins = map (foldl1 min) xss

-- *Main Day02> checksum spreadsheet1
-- 18

spreadsheet2 = [[5,9,2,8],
                [9,4,7,3],
                [3,8,6,5]]

part2 :: (Integral a) => [[a]] -> a
part2 xss = foldl1 (+) $ map checkRow xss where
  checkRow xs = foldl1 max $ quots xs
  quots xs = map (\(a,b)->quot a b) $ validPairs xs
  validPairs xs = filter validPair $ allPairs xs xs
  validPair (a,b) = (a /= b) && (0 == (rem (max a b) (min a b)))
  allPairs xs ys = concatMap (\x->(map (\y->((max x y), (min x y)))) ys) xs

-- *Main Day02> Day02.part2 spreadsheet2
-- 9
