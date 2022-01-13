module Day10 where

import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Lib (readLines)

testInput :: [String]
testInput = ["[({(<(())[]>[[{[]{<()<>>", "[(()[<>])]({[<{<<[]>>(", "{([(<{}[<>[]}>{[]{[(<()>", "(((({<>}<{<{<>}{[]{[]{}", "[[<[([]))<([[{}[[()]]]", "[{[{({}]{}}([{[{{{}}([]", "{<[[]]>}<{[{[{[]{()[[[]", "[<(<(<(<{}))><([]([]()", "<{([([[(<>()){}]>(<<{{", "<{([{{}}[<[[[<>{}]]]>[]]"]

processInput :: IO [String]
processInput = readLines 10

openers :: [Char]
openers = "[(<{"

closer :: Char -> Char
closer '[' = ']'
closer '(' = ')'
closer '{' = '}'
closer '<' = '>'
closer c = error $ "Bad symbol: " ++ [c]

findFirstIncorrect :: String -> String -> Maybe Char
findFirstIncorrect _ "" = Nothing
findFirstIncorrect queue (h : t)
  | h `elem` openers = findFirstIncorrect (h : queue) t
  | null queue = Just h
  | h == closer (head queue) = findFirstIncorrect (tail queue) t
  | otherwise = Just h

toValue1 :: Char -> Int
toValue1 ']' = 57
toValue1 ')' = 3
toValue1 '}' = 1197
toValue1 '>' = 25137
toValue1 c = error $ "Bad symbol: " ++ [c]

task1 :: [String] -> Int
task1 = sum . map toValue1 . mapMaybe (findFirstIncorrect "")

executeTask1 :: IO Int
executeTask1 = task1 <$> processInput

findUnclosed :: String -> String -> Maybe String
findUnclosed q "" = Just q
findUnclosed queue (h : t)
  | h `elem` openers = findUnclosed (h : queue) t
  | null queue = Nothing
  | h == closer (head queue) = findUnclosed (tail queue) t
  | otherwise = Nothing

toValue2 :: Char -> Int
toValue2 '[' = 2
toValue2 '(' = 1
toValue2 '{' = 3
toValue2 '<' = 4
toValue2 c = error $ "Bad symbol: " ++ [c]

task2 :: [String] -> Int
task2 =
  (\x -> x !! (length x `div` 2))
    . sort
    . map (foldl (\acc cur -> toValue2 cur + acc * 5) 0)
    . mapMaybe (findUnclosed "")

executeTask2 :: IO Int
executeTask2 = task2 <$> processInput
