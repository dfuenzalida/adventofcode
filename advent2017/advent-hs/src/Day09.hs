module Day09 where

dropAfter :: Char -> String -> String
dropAfter c xs = case xs of
  (y:ys) -> if y == c then dropAfter c (tail ys) else y : dropAfter c ys
  otherwise -> xs

drop2 :: Bool -> String -> String
drop2 xing xs = case (xs,xing) of
  (('>':ys),_)   -> drop2 False ys
  (('<':ys),_)   -> drop2 True ys
  ((y:ys),True)  -> drop2 True ys
  ((y:ys),False) -> y : drop2 False ys
  otherwise      -> xs

clean :: String -> String
clean s = drop2 False $ dropAfter '!' s

score :: String -> Integer -> Integer -> Integer
score s nesting total =
  if (null s) then total else
    case (head s) of
      '{' -> score (tail s) (nesting+1) (total+nesting)
      '}' -> score (tail s) (nesting-1) total
      otherwise -> score (tail s) nesting total

-- (score (clean "{{<a!>},{<a!>},{<a!>},{<ab>}}") 1 0) -- 3

part1 = do
    fileContents <- readFile "resources/input09.txt"
    putStrLn $ show $ score (clean fileContents) 1 0

----------------------------------------------------------------------

countGarbage :: Integer -> Bool -> String -> Integer
countGarbage n xing xs = case (xs,xing) of
  (('>':ys),_)     -> countGarbage n False ys
  (('<':ys),False) -> countGarbage n True ys
  ((y:ys),True)    -> countGarbage (n+1) True ys
  ((y:ys),False)   -> countGarbage n False ys
  otherwise        -> n

part2 = do
    fileContents <- readFile "resources/input09.txt"
    putStrLn $ show $ countGarbage 0 False $ (dropAfter '!' fileContents)
