module Day06 where

import qualified Data.Set as Set

exampleBanks = [0, 2, 7, 0]

updateAt :: [a] -> Int -> a -> [a]
updateAt xs idx val = ys ++ [val] ++ zs where
  (ys,_:zs) = splitAt idx xs

distribute :: [Integer] -> Int -> Integer -> [Integer]
distribute xs idx cnt =
  if (cnt < 1) then xs else
    distribute xs' (1+idx') (cnt-1) where
    xs' = updateAt xs idx' (1+(xs !! idx'))
    idx' = mod idx (length xs)

reallocate :: [Integer] -> Set.Set [Integer] -> Integer -> Integer
reallocate xs prevs i =
  if Set.member xs prevs then i else
    reallocate xs' (Set.insert xs prevs) (1+i) where
    maxVal = maximum xs
    maxIdx = snd $ head $ filter (\(x,_)->x == maxVal) $ zipWith (,) xs [0..]
    xs' = distribute (updateAt xs maxIdx 0) (1+maxIdx) maxVal

-- reallocate exampleBanks Set.empty 0 -> (5,5)
-- reallocate [4,10,4,1,8,4,9,14,5,1,14,15,0,15,3,5] Set.empty 0

