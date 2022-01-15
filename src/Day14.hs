module Day14 where

import Data.List (group, sort, sortOn)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Lib (readLines)

processInput :: IO (String, [(String, Char)])
processInput = do
  (initial, _ : cmds) <- splitAt 1 <$> readLines 14
  return (head initial, map (\str -> let (f : s : _) = splitOn " -> " str in (f, head s)) cmds)

applyStep1 :: [(String, Char)] -> String -> String
applyStep1 cmds current =
  concatMap
    ( \(a, b) -> case lookup [a, b] cmds of
        Just t -> [t, b]
        Nothing -> [b]
    )
    $ zip (':' : current) current

task1 :: (String, [(String, Char)]) -> Int
task1 (initial, cmds) = last counts - head counts
  where
    counts = sort $ map length $ group $ sort final
    final = iterate (applyStep1 cmds) initial !! 10

type Letters = Map.Map Char Int

type Pairs = Map.Map String Int

type CMDS = Map.Map String Char

applyPair :: CMDS -> (Letters, Pairs) -> (String, Int) -> (Letters, Pairs)
applyPair cmds (letters, pairs) (s, i) = case Map.lookup s cmds of
  Nothing -> (letters, pairs)
  Just l -> (Map.insertWith (+) l i letters, (Map.insertWith (+) [head s, l] i . Map.insertWith (+) [l, s !! 1] i) pairs)

applyStep2 :: CMDS -> (Letters, Pairs) -> (Letters, Pairs)
applyStep2 cmds (letters, pairs) = foldl (applyPair cmds) (letters, Map.empty) (Map.toList pairs)

task2 :: (String, [(String, Char)]) -> Int
task2 (s, cmds) = last counts - head counts
  where
    counts = sort $ map snd $ Map.toList $ fst $ iterate (applyStep2 (Map.fromList cmds)) (letters, pairs) !! 40
    letters = Map.fromListWith (+) $ map (\x -> (x, 1)) s
    pairs = Map.fromListWith (+) $ zipWith (\a b -> ([a, b], 1)) s $ drop 1 s
