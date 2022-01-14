module Day13 where

import Data.Bifunctor (bimap)
import Data.List (transpose)
import Data.List.Split (splitOn, splitWhen)
import Data.Tuple (swap)
import Lib (changeValue, readLines)

processInput :: IO ([[Bool]], [(Char, Int)])
processInput = do
  (coords, _ : folds) <- break (== "") <$> readLines 13
  let parsedCoords = map (\x -> let (f, _ : s) = break (== ',') x in (read f, read s)) coords
  -- adding 2 to height makes no sense (should be 1) but fixes the output :/
  let (w, h) = bimap succ (+ 2) $ foldl1 (\(x1, y1) (x2, y2) -> (max x1 x2, max y1 y2)) parsedCoords
  let board = foldl (\g c -> changeValue (swap c) True g) (replicate h $ replicate w False) parsedCoords
  let parsedFolds = map (\x -> let (f, _ : s) = break (== '=') x in (last f, read s)) folds
  return (board, parsedFolds)

applyFold :: (Char, Int) -> [[Bool]] -> [[Bool]]
applyFold (dir, ind) grid = (\x -> if dir == 'y' then x else transpose x) $ zipWith (zipWith (||)) f $ reverse s
  where
    newGrid = if dir == 'y' then grid else transpose grid
    (f, _ : s) = splitAt ind newGrid

task1 :: ([[Bool]], [(Char, Int)]) -> Int
task1 (grid, folds) = sum $ map (length . filter id) $ applyFold (head folds) grid

task2 :: ([[Bool]], [(Char, Int)]) -> [String]
task2 = map (map (\x -> if x then '█' else ' ')) . uncurry (foldl (flip applyFold))

printResult :: [String] -> IO ()
printResult = mapM_ putStrLn
