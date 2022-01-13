module Day12 where

import Data.Bifoldable (bielem, bifind)
import Data.Char (isLower)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Lib (readLines)

testInput :: [([Char], [Char])]
testInput = [("fs", "end"), ("he", "DX"), ("fs", "he"), ("start", "DX"), ("pj", "DX"), ("end", "zg"), ("zg", "sl"), ("zg", "pj"), ("pj", "he"), ("RW", "he"), ("fs", "DX"), ("pj", "RW"), ("zg", "RW"), ("start", "pj"), ("he", "WI"), ("zg", "he"), ("pj", "fs"), ("start", "RW")]

processInput :: IO [(String, String)]
processInput = map (\l -> let (h, _ : t) = break (== '-') l in (h, t)) <$> readLines 12

dfs1 :: String -> [String] -> [(String, String)] -> Int
dfs1 cur used vertices
  | cur == "end" = 1
  | otherwise =
    sum $
      map (\a -> dfs1 a newUsed vertices) $
        filter (`notElem` used) $
          mapMaybe (bifind (/= cur)) $ filter (bielem cur) vertices
  where
    newUsed = if isLower $ head cur then cur : used else used

task1 :: [(String, String)] -> Int
task1 = dfs1 "start" []

dfs2 :: String -> Bool -> [String] -> [(String, String)] -> Int
dfs2 cur twice used vertices
  | cur == "end" = 1
  | otherwise =
    sum $
      map (\a -> dfs2 a (twice || a `elem` used) newUsed vertices) $
        filter (\a -> a /= "start" && (a `notElem` used || not twice)) $
          mapMaybe (bifind (/= cur)) $ filter (bielem cur) vertices
  where
    newUsed = if isLower $ head cur then cur : used else used

task2 :: [(String, String)] -> Int
task2 = dfs2 "start" False []