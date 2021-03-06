module Day05 where

import System.IO

updateAt :: [a] -> Int -> a -> [a]
updateAt xs idx val = ys ++ [val] ++ zs where
  (ys,_:zs) = splitAt idx xs

part1loop :: Int -> [Int] -> Int -> Int
part1loop pos offsets steps =
  if (pos < 0) || (pos >= (length offsets)) then steps
  else part1loop pos' offsets' (1 + steps) where
    curr = offsets !! pos
    pos' = pos + curr
    offsets' = updateAt offsets pos (1 + curr)

-- part1loop 0 [0,3,0,1,-3] 0
-- 5

countLoops filename loopfn = result where
  readInteger s = read s :: Int
  result = do
    fileContents <- readFile filename
    let inputOffsets = map readInteger $ words fileContents
    return $ loopfn 0 inputOffsets 0

-- countLoops "resources/input05.txt" part1loop
-- 391540

----------------------------------------------------------------------

part2loop :: Int -> [Int] -> Int -> Int
part2loop pos offsets steps =
  if (pos < 0) || (pos >= (length offsets)) then steps
  else part2loop pos' offsets' (1 + steps) where
    curr = offsets !! pos
    pos' = pos + curr
    offsets' =
      updateAt offsets pos (curr + (if (curr >= 3) then (-1) else 1))

-- countLoops "resources/input05.txt" part2loop
-- 
