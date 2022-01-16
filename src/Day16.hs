module Day16 where

import Data.Char (digitToInt, intToDigit)
import Data.List (findIndex)
import Data.Maybe (fromJust)
import Lib (readLines)

processInput :: IO String
processInput = head <$> readLines 16

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, c) = (f a, c)

data Packet = Packet
  { version :: Int,
    typeID :: Int,
    lengthId :: Maybe Int,
    value :: Int,
    packets :: Maybe [Packet]
  }
  deriving (Show)

parseLiteralValue :: String -> Int -> String -> (String, String)
parseLiteralValue (f : rest) i acc
  | f == '1' = parseLiteralValue r1 (i + 5) (acc ++ c)
  | f == '0' = (acc ++ c, r1)
  | otherwise = error "Value should only contain ones and zeros"
  where
    (c, r1) = splitAt 4 rest
parseLiteralValue [] _ _ = error "Literal value cannot be empty"

parseNumberOfPackets :: Int -> String -> ([Packet], String)
parseNumberOfPackets i str = foldl (\(l, str) f -> let (p, rest) = f str in (p : l, rest)) ([], str) $ replicate i parsePacket

parseLengthOfPackets :: Int -> String -> ([Packet], String)
parseLengthOfPackets i str = list !! ind
  where
    ind = fromJust $ findIndex (\(p, str) -> length str + i == len) list
    list = iterate (\(l, str) -> let (p, rest) = parsePacket str in (p : l, rest)) ([], str)
    len = length str

parsePacket :: String -> (Packet, String)
parsePacket str = (Packet ver tId lId val ps, r5)
  where
    (ver, r1) = mapFst fromBinaryToInt $ splitAt 3 str
    (tId, r2) = mapFst fromBinaryToInt $ splitAt 3 r1
    (lId, r3) = case tId of
      4 -> (Nothing, r2)
      _ -> (Just $ digitToInt $ head r2, tail r2)
    (val, r4) = mapFst fromBinaryToInt $ case lId of
      Just n -> case n of
        0 -> splitAt 15 r3
        1 -> splitAt 11 r3
        _ -> error "lId should be 0, 1 or Nothing"
      Nothing -> parseLiteralValue r3 6 ""
    (ps, r5) = case lId of
      Nothing -> (Nothing, r4)
      Just n -> case n of
        0 -> mapFst Just $ parseLengthOfPackets val r4
        1 -> mapFst Just $ parseNumberOfPackets val r4
        _ -> error "Not implemented"

toBinaryString :: String -> String
toBinaryString = concatMap (tail . bits "" . (+ 16) . digitToInt)
  where
    bits a 0 = a
    bits a i = bits (intToDigit (i `mod` 2) : a) (i `div` 2)

fromBinaryToInt :: String -> Int
fromBinaryToInt = foldl (\acc c -> acc * 2 + digitToInt c) 0

sumVersions :: Packet -> Int
sumVersions (Packet v _ _ _ Nothing) = v
sumVersions (Packet v _ _ _ (Just l)) = v + sum (map sumVersions l)

task1 :: String -> Int
task1 str = sumVersions $ fst $ parsePacket $ toBinaryString str

calculatePacket :: Packet -> Int
calculatePacket (Packet _ _ _ v Nothing) = v
calculatePacket (Packet _ t _ v (Just l))
  | t == 0 = sum values
  | t == 1 = product values
  | t == 2 = minimum values
  | t == 3 = maximum values
  | t == 5 = if head values < values !! 1 then 1 else 0
  | t == 6 = if head values > values !! 1 then 1 else 0
  | t == 7 = if head values == values !! 1 then 1 else 0
  | otherwise = error "Not possible packet type"
  where
    values = map calculatePacket l

task2 :: String -> Int
task2 str = calculatePacket $ fst $ parsePacket $ toBinaryString str