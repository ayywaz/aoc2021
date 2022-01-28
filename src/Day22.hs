module Day22 where

import Data.Bifunctor (Bifunctor (second))
import Data.Foldable (foldl')
import Data.List.Split (splitOn)
import Lib (readLines)

parseLine :: String -> (String, [(Int, Int)])
parseLine line = (com, map ((\l -> (head l, l !! 1)) . map read . splitOn ".." . drop 2) $ splitOn "," rest)
  where
    (com, _ : rest) = break (== ' ') line

processInput :: IO [(String, [(Int, Int)])]
processInput = map parseLine <$> readLines 22

inRange :: Int -> (Int, Int) -> Bool
inRange val (l, h) = l <= val && val <= h

isOverlapping :: (Int, Int) -> (Int, Int) -> Bool
isOverlapping b1@(l1, h1) b2@(l2, h2) = inRange l1 b2 || inRange h1 b2 || inRange l2 b1

overlap :: (Int, Int) -> (Int, Int) -> (Int, Int)
overlap (l1, h1) (l2, h2) = (max l1 l2, min h1 h2)

applyStep :: [(String, [(Int, Int)])] -> (String, [(Int, Int)]) -> [(String, [(Int, Int)])]
applyStep acc (s, cur) = ([(s, cur) | s == "on"]) ++ overlaps ++ acc
  where
    overlaps =
      map (\(str, l) -> (if str == "on" then "off" else "on", zipWith overlap l cur)) $
        filter (\(_, l) -> and (zipWith isOverlapping l cur)) acc

cubeVolume :: [(Int, Int)] -> Int
cubeVolume = product . map (succ . negate . uncurry (-))

addCubes :: [(String, [(Int, Int)])] -> Int
addCubes = sum . map (\(s, l) -> cubeVolume l * if s == "on" then 1 else -1)

task1 :: [(String, [(Int, Int)])] -> Int
task1 steps = addCubes $ foldl' applyStep [] squeezed
  where
    filtered = filter (\(_, l) -> all (isOverlapping (-50, 50)) l) steps
    squeezed = map (second (map $ overlap (-50, 50))) filtered

task2 :: [(String, [(Int, Int)])] -> Int
task2 steps = addCubes $ foldl' applyStep [] steps
