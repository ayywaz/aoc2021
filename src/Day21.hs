{-# LANGUAGE MultiWayIf #-}

module Day21 where

import Data.Bifunctor (Bifunctor (bimap, second))
import Data.List (partition)
import qualified Data.Map as M
import Data.Tuple (swap)
import Lib (readLines)
import State (State (runState), evalState, get, put)

processInput :: IO (Int, Int)
processInput = do
  lines <- readLines 21
  let list = map (read . drop 2 . dropWhile (/= ':')) lines
  return (head list, list !! 1)

makeTurn1 :: Int -> [Int] -> (Int, [Int])
makeTurn1 space vals = let (f, l) = splitAt 3 vals in ((space - 1 + sum f) `mod` 10 + 1, l)

playGame1 :: (Int, Int) -> (Int, Int) -> Int -> [Int] -> (Int, Int)
playGame1 (p1, p2) (s1, s2) n die
  | s1 + newP1 >= 1000 = (s2, n + 3)
  | s2 + newP2 >= 1000 = (s1 + newP1, n + 6)
  | otherwise = playGame1 (newP1, newP2) (s1 + newP1, s2 + newP2) (n + 6) newerDie
  where
    (newP1, newDie) = makeTurn1 p1 die
    (newP2, newerDie) = makeTurn1 p2 newDie

type Die = State [Int]

makeTurn1' :: Int -> Die Int
makeTurn1' place = do
  (f, l) <- splitAt 3 <$> get
  put l
  return $ (place - 1 + sum f) `mod` 10 + 1

playGame1' :: (Int, Int) -> (Int, Int) -> Int -> Die (Int, Int)
playGame1' (p1, p2) (s1, s2) n = do
  newP1 <- makeTurn1' p1
  newP2 <- makeTurn1' p2
  if
      | s1 + newP1 >= 1000 -> return (s2, n + 3)
      | s2 + newP2 >= 1000 -> return (s1 + newP1, n + 6)
      | otherwise -> playGame1' (newP1, newP2) (s1 + newP1, s2 + newP2) (n + 6)

task1 :: (Int, Int) -> Int
task1 p = uncurry (*) $ playGame1 p (0, 0) 0 (cycle [1 .. 100])

task1' :: (Int, Int) -> Int
task1' p = uncurry (*) $ evalState (playGame1' p (0, 0) 0) (cycle [1 .. 100])

type Score = Int

type Place = Int

type Count = Int

type Dict = M.Map (Score, Place) Count

type Entries = State (Dict, Dict)

makeTurn2 :: Entries (Int, Int)
makeTurn2 = do
  (player1, player2) <- get
  if M.null player2
    then return (0, 0)
    else do
      let (lessP1, moreP1) =
            partition (\((s, _), _) -> s < 21) $
              concatMap
                ( \((s, p), c) ->
                    map (\(v, a) -> let newP = (p - 1 + v) `mod` 10 + 1 in ((s + newP, newP), c * a)) template
                )
                $ M.toList player1
      put (player2, M.fromListWith (+) lessP1)
      second (+ sum (M.elems player2) * sum (map snd moreP1)) . swap <$> makeTurn2
  where
    template = [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)]

task2 :: (Int, Int) -> Int
task2 (p1, p2) = uncurry max $ evalState makeTurn2 (M.singleton (0, p1) 1, M.singleton (0, p2) 1)