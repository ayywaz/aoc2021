module Day11 where

import Control.Monad (replicateM, when)
import Data.Char (digitToInt)
import Data.List (elemIndex)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Lib (modifyValue, readInput)
import State (State, evalState, get, modify)

testInput :: [[Char]]
testInput = ["5483143223", "2745854711", "5264556173", "6141336146", "6357385478", "4167524645", "2176841721", "6882881134", "4846848554", "5283751526"]

processInput :: IO [String]
processInput = splitOn "\n" <$> readInput 11

type Grid = State [[Int]]

getNeighbors :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
getNeighbors (x, y) (h, w) =
  [ (i, j)
    | i <- [pred x .. succ x],
      i >= 0 && i < h,
      j <- [pred y .. succ y],
      j >= 0 && j < w,
      i /= x || j /= y
  ]

getSize :: [[a]] -> (Int, Int)
getSize [] = (0, 0)
getSize l = (length l, length $ head l)

dfs :: (Int, Int) -> (Int, Int) -> Grid ()
dfs (x, y) (h, w) = do
  s <- get
  let cur = s !! x !! y
  when (cur <= 9) $ modify $ modifyValue (x, y) (+ 1)
  when (cur == 9) $
    mapM_ (\c -> dfs c (h, w)) $ getNeighbors (x, y) (h, w)

applyStep :: [(Int, Int)] -> (Int, Int) -> Grid Int
applyStep coords bounds = do
  mapM_ (`dfs` bounds) coords
  s <- get
  let ans = sum $ map (length . filter (== 10)) s
  modify (map (map $ \x -> if x /= 10 then x else 0))
  return ans

task1 :: [[Char]] -> Int
task1 charGrid = sum $ evalState (replicateM 100 (applyStep coords (h, w))) grid
  where
    grid = map (map digitToInt) charGrid
    (h, w) = getSize grid
    coords = [(i, j) | i <- [0 .. h -1], j <- [0 .. w -1]]

task2 :: [[Char]] -> Int
task2 charGrid = succ $ fromJust $ elemIndex 100 $ evalState (sequence $ repeat (applyStep coords (h, w))) grid
  where
    grid = map (map digitToInt) charGrid
    (h, w) = getSize grid
    coords = [(i, j) | i <- [0 .. h -1], j <- [0 .. w -1]]
