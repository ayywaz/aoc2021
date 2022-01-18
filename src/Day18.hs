module Day18 where

import Data.Char (isDigit)
import Data.List (find)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Lib (readLines)

data SNum = Lit Int | Pair SNum SNum deriving (Eq)

instance Show SNum where
  show (Lit i) = show i
  show (Pair a b) = concat ["[", show a, ",", show b, "]"]

testInput :: [Char]
testInput = "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"

parseSNum :: String -> (SNum, String)
parseSNum [] = error "not a snail number"
parseSNum str
  | isDigit $ head str = let (n, rest) = span isDigit str in (Lit $ read n, rest)
  | head str == ',' = parseSNum $ tail str
  | head str == '[' =
    let (f, rest) = parseSNum $ tail str
        (s, rest2) = parseSNum rest
     in (Pair f s, rest2)
  | head str == ']' = parseSNum $ tail str
  | otherwise = error $ "unknown character for snail number" ++ [head str]

explodePair :: Int -> Maybe Int -> Maybe Int -> SNum -> (Maybe SNum, (Maybe Int, Maybe Int))
explodePair 4 Nothing Nothing (Pair (Lit a) (Lit b)) = (Just $ Lit 0, (Just a, Just b))
explodePair _ l r (Lit a)
  | Just n <- l = (Just $ Lit $ a + n, (Nothing, Nothing))
  | Just n <- r = (Just $ Lit $ a + n, (Nothing, Nothing))
  | otherwise = (Nothing, (Nothing, Nothing))
explodePair d l r (Pair a b)
  | isJust l = (Just $ Pair a (fromJust newB), (Nothing, Nothing))
  | isJust r = (Just $ Pair (fromJust newA) b, (Nothing, Nothing))
  | isJust newA = (Just $ Pair (fromJust newA) (if isNothing rA then b else fromJust newerB), (lA, r))
  | isJust newB = (Just $ Pair (if isNothing lB then a else fromJust newerA) (fromJust newB), (l, rB))
  | otherwise = (Nothing, (l, r))
  where
    (newA, (lA, rA)) = explodePair (succ d) l r a
    (newB, (lB, rB)) = explodePair (succ d) l r b
    (newerA, _) = explodePair (succ d) lB r a
    (newerB, _) = explodePair (succ d) l rA b

explodePair' :: Int -> (Int, Int) -> SNum -> (SNum, (Int, Int))
explodePair' 4 (x, y) (Pair (Lit a) (Lit b)) = (Lit 0, (a + x, b + y))
explodePair' _ (x, y) (Lit a) = (Lit $ a + x + y, (0, 0))
explodePair' d (x, y) (Pair a b) = (Pair newerA newB, (lA, rB))
  where
    (newA, (lA, rA)) = explodePair' (succ d) (x, 0) a
    (newB, (lB, rB)) = explodePair' (succ d) (rA, y) b
    newerA = changeValue' False lB newA

changeValue' :: Bool -> Int -> SNum -> SNum
changeValue' _ val (Lit a) = Lit $ a + val
changeValue' isLeft val (Pair a b)
  | isLeft = Pair (changeValue' isLeft val a) b
  | otherwise = Pair a (changeValue' isLeft val b)

splitPair :: SNum -> Maybe SNum
splitPair (Lit a) = if a >= 10 then Just $ Pair (Lit $ floor (fromIntegral a / 2)) (Lit $ ceiling (fromIntegral a / 2)) else Nothing
splitPair (Pair a b)
  | isJust newA = Just $ Pair (fromJust newA) b
  | isJust newB = Just $ Pair a (fromJust newB)
  | otherwise = Nothing
  where
    newA = splitPair a
    newB = splitPair b

processInput :: IO [SNum]
processInput = map (fst . parseSNum) <$> readLines 18

reduce :: SNum -> SNum
reduce n = snd $ fromJust $ find fst $ iterate (split . explode . snd) (False, n)
  where
    explode c = maybe c explode $ fst $ explodePair 0 Nothing Nothing c
    split c = maybe (True, c) (\newC -> (False, newC)) $ splitPair c

reduce' :: SNum -> SNum
reduce' n = snd $ fromJust $ find fst $ iterate (split . fst . explodePair' 0 (0, 0) . snd) (False, n)
  where
    split c = maybe (True, c) (\newC -> (False, newC)) $ splitPair c

magnitude :: SNum -> Int
magnitude (Lit a) = a
magnitude (Pair a b) = 3 * magnitude a + 2 * magnitude b

task1 :: [SNum] -> Int
task1 = magnitude . foldl1 (\a b -> reduce $ Pair a b)

task2 :: [SNum] -> Int
task2 l = maximum [magnitude (reduce (Pair a b)) | a <- l, b <- l, a /= b]

task2' :: [SNum] -> Int
task2' l = maximum [magnitude (reduce' (Pair a b)) | a <- l, b <- l, a /= b]