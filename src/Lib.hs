module Lib where

readInput :: Int -> IO String
readInput i = readFile $ "input/Day" ++ (if length (show i) == 1 then "0" else "") ++ show i ++ ".txt"

readLines :: Int -> IO [String]
readLines = fmap lines . readInput

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