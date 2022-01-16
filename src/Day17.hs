module Day17 where

import Data.Char (isDigit)
import Data.List (group, sort)
import Lib (readLines)

testInput :: ((Int, Int), (Int, Int))
testInput = ((20, 30), (-10, -5))

processInput :: IO ((Int, Int), (Int, Int))
processInput = do
  numbers <- map read . words . map (\x -> if isDigit x || x == '-' then x else ' ') . head <$> readLines 17
  return ((head numbers, numbers !! 1), (numbers !! 2, numbers !! 3))

task1 :: ((Int, Int), (Int, Int)) -> Int
task1 (_, (a, _)) = (a + 1) * a `div` 2

generateX :: (Int, Int) -> Int -> [Int] -> [[Int]]
generateX (l, h) steps inf
  | null values = repeat inf
  | otherwise = (inf ++ values) : generateX (l, h) (succ steps) (if head values == steps then steps : inf else inf)
  where
    decrement = steps * pred steps `div` 2
    values = dropWhile (< steps) $ map (`div` steps) [lower, lower + steps .. h + decrement]
    lower = (negate (l + decrement) `mod` steps) + l + decrement

generateY :: (Int, Int) -> Int -> [[Int]]
generateY (l, h) steps = values : generateY (l, h) (succ steps)
  where
    decrement = steps * pred steps `div` 2
    values = map (`div` steps) [lower, lower + steps .. h + decrement]
    lower = (negate (l + decrement) `mod` steps) + l + decrement

task2 :: ((Int, Int), (Int, Int)) -> Int
task2 (x, y) = length $ group $ sort $ concat $ take (negate (fst y) * 2) $ zipWith (\x y -> [(i, j) | i <- x, j <- y]) (generateX x 1 []) $ generateY y 1