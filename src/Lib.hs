module Lib where

readInput :: Int -> IO String
readInput i = readFile $ "input/Day" ++ show i ++ ".txt"
