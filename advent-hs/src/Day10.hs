module Day10 where

import Data.Char (ord, chr)
import Data.Bits (xor)

updateIn :: [a] -> Int -> a -> [a]
updateIn xs i z = take i xs ++ [z] ++ drop (i+1) xs

-- substitute elements from one list into the other with a given offset
updateVec :: [a] -> [a] -> Int -> Int -> [a]
updateVec v1 v2 offset i =
  if null v2 then v1 else
    updateVec v1' (tail v2) offset (i+1) where
    v1' = (updateIn v1 (mod (i+offset) (length v1)) (head v2))

iter :: [Int] -> [Int] -> Int -> Int -> [Int]
iter elems lengths pos skip =
  if null lengths then elems else
    iter uptd (tail lengths) (mod (pos + fstlen + skip) l) (skip+1) where
    fstlen = head lengths
    l = length elems
    revd = reverse $ take fstlen $ drop pos $ cycle elems
    uptd = updateVec elems revd pos 0

-- iter [0,1,2,3,4] [3,4,1,5] 0 0

input = [97,167,54,178,2,11,209,174,119,248,254,0,255,1,64,190]

part1 = (head nums) * (nums !! 1) where
  nums = iter [0..255] input 0 0

----------------------------------------------------------------------

stringToLen :: String -> [Int]
stringToLen s = map ord s ++ [17, 31, 73, 47, 23]

iter2 elems lengths pos skip =
  if null lengths then (elems,pos,skip) else
    iter2 uptd (tail lengths) (mod (pos + fstlen + skip) l) (skip+1) where
    fstlen = head lengths
    l = length elems
    revd = reverse $ take fstlen $ drop pos $ cycle elems
    uptd = updateVec elems revd pos 0

runRounds elems lengths = let
  loopRounds = \e p s rounds -> if (rounds < 1) then e else
    let (e2,p2,s2) = iter2 e lengths p s
    in loopRounds e2 p2 s2 (rounds-1) where
  in loopRounds elems 0 0 64

partition n xs = if (length xs <= n) then [xs]
  else [(take n xs)] ++ partition n (drop n xs)

denseHash xs = map (foldl1 xor) $ partition 16 xs

toHex n = [digits !! (quot n 16)] ++ [digits !! (mod n 16)] where
  digits = "0123456789abcdef"

knotHash :: String -> String
knotHash s = concatMap toHex densed where
  lengths = stringToLen s
  elems = runRounds [0..255] lengths
  densed = denseHash elems

part2 = knotHash $ filter ((<64) . ord) $ show input -- drop '[' and ']'
