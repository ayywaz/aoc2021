module Day20 where

import Data.Char (digitToInt)
import Data.List (foldl', transpose)
import Lib (readLines)

processInput :: IO (String, [String])
processInput = do
  (f : _ : rest) <- readLines 20
  return (f, rest)

appendBeforeAndAfter :: [a] -> [a] -> [a]
appendBeforeAndAfter item list = concat [item, list, item]

zipWithNeighbors :: (a -> a -> a -> b) -> [[a]] -> [[b]]
zipWithNeighbors f grid =
  zipWith3
    (zipWith3 f)
    grid
    (tail grid)
    (tail $ tail grid)

toDec :: String -> Int
toDec = foldl' (\acc cur -> acc * 2 + if cur == '.' then 0 else 1) 0

applyStep :: Char -> String -> [String] -> [String]
applyStep surrounding mask grid =
  zipWithNeighbors (\a b c -> mask !! toDec (concat [a, b, c])) $
    transpose $ zipWithNeighbors (\a b c -> [a, b, c]) $ transpose newGrid
  where
    extraRow = replicate (4 + length (head grid)) surrounding
    newGrid =
      appendBeforeAndAfter [extraRow, extraRow] $
        map (appendBeforeAndAfter [surrounding, surrounding]) grid

applySteps :: Int -> String -> [String] -> [String]
applySteps n mask grid =
  fst $
    foldl
      (\(g, c) i -> (applyStep c mask g, if c == '.' then head mask else last mask))
      (grid, '.')
      [1 .. n]

task1 :: (String, [String]) -> Int
task1 (mask, grid) = sum $ map (sum . map (\x -> if x == '.' then 0 else 1)) $ applySteps 2 mask grid

task2 :: (String, [String]) -> Int
task2 (mask, grid) = sum $ map (sum . map (\x -> if x == '.' then 0 else 1)) $ applySteps 50 mask grid