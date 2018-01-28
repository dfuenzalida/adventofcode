module Day07 where

import Data.Map as Map hiding (filter, foldl, map, null, take)

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
  parents = Map.foldr (:) [] pm

part1 = do
    fileContents <- readFile "resources/input07.txt"
    putStrLn $ findRoot $ lines fileContents

----------------------------------------------------------------------
