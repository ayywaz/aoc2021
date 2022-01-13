module Day09 where

import Control.Monad (when)
import Data.Char (digitToInt)
import Data.List (sortBy, transpose)
import Data.List.Split (splitOn)
import Lib (changeValue, readLines)
import State (State (State, runState), evalState, get, put, runState)

testInput :: [String]
testInput = ["2199943210", "3987894921", "9856789892", "8767896789", "9899965678"]

processInput :: IO [String]
processInput = readLines 9

isFirstOneSmallest :: Ord a => a -> a -> a -> Bool
isFirstOneSmallest a b c
  | a < b && a < c = True
  | otherwise = False

smallestAmongAdjacentMap :: Ord a => a -> [[a]] -> [[Bool]]
smallestAmongAdjacentMap corner grid = zipWith3 (zipWith3 isFirstOneSmallest) grid shiftL shiftR
  where
    shiftL = map (flip (++) [corner] . tail) grid
    shiftR = map ((++) [corner] . init) grid

task1 :: [[Char]] -> Int
task1 grid = sum $ map (foldl (\a (x, b1, b2) -> a + if b1 && b2 then digitToInt x + 1 else 0) 0) $ zipWith3 zip3 grid m1 m2
  where
    m1 = smallestAmongAdjacentMap ':' grid
    m2 = transpose $ smallestAmongAdjacentMap ':' $ transpose grid

executeTask1 :: IO Int
executeTask1 = task1 <$> processInput

type Visited = State [[Bool]]

gridCharToBool :: [[Char]] -> [[Bool]]
gridCharToBool = map (map (== '9'))

markTrue :: (Int, Int) -> (Int, Int) -> Visited Bool
markTrue (x, y) (h, w) = do
  s <- get
  if s !! x !! y
    then return False
    else do
      put $ changeValue (x, y) True s
      return True

getSize :: [[a]] -> (Int, Int)
getSize [] = (0, 0)
getSize l = (length l, length $ head l)

getNeighbors :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
getNeighbors (x, y) (h, w) = [(x + i, y + j) | (i, j) <- template, x + i >= 0 && x + i < h, y + j >= 0 && y + j < w]
  where
    template = [(0, 1), (0, -1), (-1, 0), (1, 0)]

dfs :: (Int, Int) -> (Int, Int) -> Visited Int
dfs (x, y) (h, w) = do
  s <- get
  mark <- markTrue (x, y) (h, w)
  if mark
    then do
      counts <- mapM (\c -> dfs c (h, w)) $ getNeighbors (x, y) (h, w)
      return $ 1 + sum counts
    else return 0

traverseVisited :: Visited [Int]
traverseVisited = State $ \s ->
  let (h, w) = getSize s
      coords = [(i, j) | i <- [0 .. h -1], j <- [0 .. w -1]]
   in runState (mapM (\c -> dfs c (h, w)) coords) s

task2 :: [String] -> Int
task2 = product . take 3 . sortBy (flip compare) . evalState traverseVisited . gridCharToBool

executeTask2 :: IO Int
executeTask2 = task2 <$> processInput