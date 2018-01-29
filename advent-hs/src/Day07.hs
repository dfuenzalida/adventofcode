module Day07 where

import Data.Map as Map hiding (filter, foldl, foldr, map, null, take)
import qualified Data.Map as DMap

exampleInput = ["pbga (66)",
                "xhth (57)",
                "ebii (61)",
                "havc (66)",
                "ktlj (57)",
                "fwft (72) -> ktlj, cntj, xhth",
                "qoyq (66)",
                "padx (45) -> pbga, havc, qoyq",
                "tknk (41) -> ugml, padx, fwft",
                "jptl (61)",
                "ugml (68) -> gyxo, ebii, jptl",
                "gyxo (61)",
                "cntj (57)"]

data Node = Node String Integer [String]
  deriving (Eq, Show)

readInt s = read s :: Integer

zap :: String -> [String] -> [String] -- removes any cs in xs
zap cs xs = map (filter (not . ((flip elem) cs))) xs

wordsToNode :: [String] -> Node
wordsToNode (name:weight:[]) = Node name (readInt weight) []
wordsToNode (name:weight:_:xs) = Node name (readInt weight) (zap "," xs)

toKVpairs :: Node -> [(String, String)]
toKVpairs (Node n _ xs) = zipWith (,) xs (repeat n) -- Note: child to parent

parentsMap :: [String] -> Map String String
parentsMap ls = fromList $ concatMap (toKVpairs . wordsToNode . words) ls

-- the root is a value from the map that is not a member (key) of the map
findRoot :: [String] -> String
findRoot lines = head $ filter (not . ((flip member) pm)) parents where
  pm = parentsMap lines
  parents = DMap.foldr (:) [] pm

part1 = do
    fileContents <- readFile "resources/input07.txt"
    putStrLn $ findRoot $ lines fileContents

----------------------------------------------------------------------

foldNodes :: Map String Node -> String -> Map String Node
foldNodes m s = insert (head ws) (wordsToNode ws) m where ws = words s

nodesMap :: [String] -> Map String Node
nodesMap input = foldl foldNodes empty input

children (Node _ _ xs) = xs

weight :: Map String Node -> String -> Integer
weight m name =
  (+ (nw curr)) $ foldl (+) 0 $ map (weight m) (children curr) where
  nw (Node _ w _) = w
  curr = findWithDefault (Node "x" 0 []) name m

-- weight (nodesMap exampleInput) "ugml" -- 251

-- groupBy even [1..5] -> fromList [(False,[1,3,5]),(True,[2,4])]
groupBy :: Ord b => (a -> b) -> [a] -> Map b [a]
groupBy f xs =
  foldr (\x m -> insert (f x) (x:(findWithDefault [] (f x) m)) m) empty xs

-- unbalanced: for a given map and root, find the unbalanced node.
-- group children by weight, filter the values that are lists of length 1,
-- take the first, extract the actual value

unbalanced :: Map String Node -> String -> String
unbalanced m n = let
  cNames = children $ findWithDefault (Node "" 0 []) n m
  unbParent = (cNames == []) ||
              ((==1) $ length $ fromList $ map (\c->(weight m c, 0)) cNames)
  in if unbParent then n else
  extract $ DMap.filter((==1) . length) $ groupBy (weight m) cNames where
  extract = head . snd . head . toList

-- unbalanced (nodesMap exampleInput) (findRoot exampleInput) -- "ugml"
