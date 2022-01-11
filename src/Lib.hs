module Lib where

-- Add readLines which splits lines

readInput :: Int -> IO String
readInput i = readFile $ "input/Day" ++ show i ++ ".txt"

changeValue :: (Int, Int) -> a -> [[a]] -> [[a]]
changeValue (x, y) val grid = prevRows ++ [prevElems ++ [val] ++ nextElems] ++ nextRows
  where
    (prevRows, curRow : nextRows) = splitAt x grid
    (prevElems, _ : nextElems) = splitAt y curRow

modifyValue :: (Int, Int) -> (a -> a) -> [[a]] -> [[a]]
modifyValue (x, y) f grid = prevRows ++ [prevElems ++ [f el] ++ nextElems] ++ nextRows
  where
    (prevRows, curRow : nextRows) = splitAt x grid
    (prevElems, el : nextElems) = splitAt y curRow