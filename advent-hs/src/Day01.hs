module Day01 where

part1 :: [Char] -> Integer
part1 s = foldl (+) 0 matchingInts where
  matching z xs ys = zipWith (\a b -> if (a == b) then [a] else z) xs ys
  digitToInt s = (read s) :: Integer
  cycle1 xs = take (length xs) $ (drop 1 (cycle xs))
  matchingInts = map digitToInt $ matching "0" s (cycle1 s)

-- *Main Day01> part1 "1122"
-- 3
-- *Main Day01> part1 "1111"
-- 4
-- *Main Day01> part1 "1234"
-- 0
-- *Main Day01> part1 "91212129"
-- 9

part2 :: [Char] -> Integer
part2 s = foldl (+) 0 matchingInts where
  matching z xs ys = zipWith (\a b -> if (a == b) then [a] else z) xs ys
  digitToInt s = (read s) :: Integer
  half = quot (length s) 2
  cycleN xs n = take (length xs) $ (drop n (cycle xs))
  matchingInts = map digitToInt $ matching "0" s (cycleN s half)

-- *Main Day01> part2 "1212"
-- 6
-- *Main Day01> part2 "1221"
-- 0
-- *Main Day01> part2 "123425"
-- 4
-- *Main Day01> part2 "123123"
-- 12
-- *Main Day01> part2 "12131415"
-- 4
