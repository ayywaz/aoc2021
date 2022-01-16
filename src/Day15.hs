module Day15 where

import Data.Char (digitToInt)
import Data.List (transpose)
import qualified Data.Set as S
import qualified Data.Vector as V
import Lib (readLines)
import State (State, put)

testInput :: [[Char]]
testInput = ["1163751742", "1381373672", "2136511328", "3694931569", "7463417111", "1319128137", "1359912421", "3125421639", "1293138521", "2311944581"]

processInput :: IO [String]
processInput = readLines 15

changeValue :: (Int, Int) -> a -> V.Vector (V.Vector a) -> V.Vector (V.Vector a)
changeValue (y, x) val grid = grid V.// [(y, grid V.! y V.// [(x, val)])]

generateNeighbors :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
generateNeighbors (h, w) (y, x) = [(y+i, x+j) | (i, j) <- template, y+i < h && y+i >= 0, x+j < w && x+j >= 0]
  where template = [(0, 1), (0, -1), (1, 0), (-1, 0)]

findLowestRisk :: (Int, Int) -> V.Vector (V.Vector Int) -> V.Vector (V.Vector Bool) -> S.Set (Int, (Int, Int)) -> Int
findLowestRisk bounds@(h, w) v visited s
  | coords == (pred h, pred w) = price
  | visited V.! y V.! x = findLowestRisk bounds v visited newS
  | otherwise = findLowestRisk bounds v newVisited $ foldr (S.insert . (\c -> (price + v V.! fst c V.! snd c, c)))  newS $ filter (\(y, x) -> not $ visited V.! y V.! x) $ generateNeighbors bounds coords
  where
    newVisited = changeValue coords True visited
    ((price, coords@(y, x)), newS) = S.deleteFindMin s

task1 :: [String] -> Int
task1 grid = findLowestRisk (h,w) vector visited s
  where
    (h, w) = (length grid, length $ head grid)
    visited = V.replicate h $ V.replicate w False
    s = S.singleton (0, (0, 0))
    vector = V.fromList $ map (V.fromList . map digitToInt) grid

nextTile :: [[Int]] -> [[Int]]
nextTile = map (map (\x -> if x == 9 then 1 else succ x))

task2 :: [String] -> Int
task2 preGrid = findLowestRisk (h, w) vector visited s
  where
    (h, w) = (length grid, length $ head grid)
    visited = V.replicate h $ V.replicate w False
    s = S.singleton (0, (0, 0))
    vector = V.fromList $ map V.fromList grid
    grid = concat $ take 5 $ iterate nextTile $ transpose $ concat $ take 5 $ iterate nextTile $ transpose $ map (map digitToInt) preGrid