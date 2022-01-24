module Day19 where

import Data.List (find, sort, sortOn)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Set as S
import Lib (readLines)
import State (State, get, put)

type Coords = [Int]

type Distances = [Int]

type Vertex = (Coords, Coords)

processInput :: IO [[Coords]]
processInput = map (map (map read . splitOn ",") . tail) . splitOn [""] <$> readLines 19

getDistances :: [Coords] -> [(Vertex, Distances)]
getDistances cs = sortOn snd [((c1, c2), sort $ zipWith ((abs .) . (-)) c1 c2) | c1 <- cs, c2 <- cs, c1 < c2]

intersectionOne :: (Vertex, Distances) -> [(Vertex, Distances)] -> [(Vertex, Vertex)]
intersectionOne (v1, d1) rest = map (\(v2, _) -> (v1, v2)) $ takeWhile (\(_, d2) -> d2 == d1) rest

intersection :: [(Vertex, Distances)] -> [(Vertex, Distances)] -> [(Vertex, Vertex)]
intersection ((c1, d1) : cs1) ((c2, d2) : cs2)
  | d1 < d2 = intersection cs1 ((c2, d2) : cs2)
  | d1 > d2 = intersection ((c1, d1) : cs1) cs2
  | otherwise = concat [[(c1, c2)], intersectionOne (c1, d1) cs2, intersectionOne (c2, d2) cs1, intersection cs1 cs2]
intersection _ _ = []

type Permutation = [(Int, Int)]

cubePermutations :: [Permutation]
cubePermutations =
  [zip i c | i <- f, c <- [[1, 1, 1], [1, -1, -1], [-1, 1, -1], [-1, -1, 1]]]
    ++ [zip i c | i <- s, c <- [[1, 1, -1], [1, -1, 1], [-1, -1, -1], [-1, 1, 1]]]
  where
    f = [[0, 1, 2], [1, 2, 0], [2, 0, 1]]
    s = [[0, 2, 1], [2, 1, 0], [1, 0, 2]]

applyPermutation :: Permutation -> [Int] -> [Int]
applyPermutation p cs = map (\(i, c) -> (cs !! i) * c) p

isMatching :: Coords -> Coords -> Bool
isMatching d1 d2 = and (zipWith (==) d1 d2) || and (zipWith (\a b -> (- a) == b) d1 d2)

equalOn :: Eq b => (a -> b) -> a -> a -> Bool
equalOn f a b = f a == f b

findPermutation :: [(Vertex, Vertex)] -> Maybe (Permutation, Coords)
findPermutation vs =
  fmap (\(a, b) -> let (c1, c2) = head b in (a, zipWith (-) (fst c1) ((if equalOn (uncurry (zipWith (-))) c1 c2 then fst else snd) c2))) $
    find ((>= 66) . length . snd) $
      sortOn length $
        map
          ( \p ->
              ( p,
                filter
                  (\(f, s) -> isMatching (uncurry (zipWith (-)) f) (uncurry (zipWith (-)) s))
                  $ map (\(f, (s1, s2)) -> (f, (applyPermutation p s1, applyPermutation p s2))) vs
              )
          )
          cubePermutations

type Nodes = State [([Coords], Bool)]

-- Computes incorrect result (compares 2 scanners at a time)
dfs :: Int -> Maybe (Permutation, Coords) -> Nodes ()
dfs _ Nothing = pure ()
dfs index (Just (p, c)) = do
  s <- get
  let (prev, (coords, _) : next) = splitAt index s
  let newCoords = map (zipWith (+) c . applyPermutation p) coords
  put (prev ++ [(newCoords, True)] ++ next)
  mapM_ (\((c, _), i) -> dfs i (findPermutation $ intersection (getDistances newCoords) (getDistances c))) $ filter (\((_, b), i) -> i /= index && not b) $ zip s [0 ..]

-- Computes correct result (compares each unknown scanner with a pool of all known scanners)
-- Could be optimised to save pass on vertices and distances instead of recalculating them
noDfs1 :: [[Coords]] -> Int -> S.Set Coords -> [Bool] -> (Permutation, Coords) -> S.Set Coords
noDfs1 grid ind coords visited (permutation, offset)
  | null b = newCoords
  | otherwise = noDfs1 grid (fst $ head b) newCoords newVisited (snd $ head b)
  where
    b = mapMaybe (\i -> fmap (\a -> (i, a)) $ findPermutation $ intersection newDistances (getDistances $ grid !! i)) a
    a = map snd $ filter (\(b, _) -> not b) $ zip newVisited [0 ..]
    (prev, _ : next) = splitAt ind visited
    newVisited = prev ++ [True] ++ next
    newCoords = S.union coords $ S.fromList $ map (zipWith (+) offset . applyPermutation permutation) (grid !! ind)
    newDistances = getDistances $ S.toList newCoords

noDfs2 :: [[Coords]] -> Int -> S.Set Coords -> [Bool] -> (Permutation, Coords) -> [Coords]
noDfs2 grid ind coords visited (permutation, offset)
  | null b = [offset]
  | otherwise = offset : noDfs2 grid (fst $ head b) newCoords newVisited (snd $ head b)
  where
    b = mapMaybe (\i -> fmap (\a -> (i, a)) $ findPermutation $ intersection newDistances (getDistances $ grid !! i)) a
    a = map snd $ filter (\(b, _) -> not b) $ zip newVisited [0 ..]
    (prev, _ : next) = splitAt ind visited
    newVisited = prev ++ [True] ++ next
    newCoords = S.union coords $ S.fromList $ map (zipWith (+) offset . applyPermutation permutation) (grid !! ind)
    newDistances = getDistances $ S.toList newCoords

task1 :: [[Coords]] -> Int
task1 cs = length corrected
  where
    corrected = noDfs1 cs 0 S.empty (replicate (length cs) False) (head cubePermutations, [0, 0, 0])

task2 :: [[Coords]] -> Int
task2 cs = maximum [sum $ zipWith ((abs .) . (-)) c1 c2 | c1 <- corrected, c2 <- corrected, c1 > c2]
  where
    corrected = noDfs2 cs 0 S.empty (replicate (length cs) False) (head cubePermutations, [0, 0, 0])

testInput :: [[Coords]]
testInput =
  [ [[404, -588, -901], [528, -643, 409], [-838, 591, 734], [390, -675, -793], [-537, -823, -458], [-485, -357, 347], [-345, -311, 381], [-661, -816, -575], [-876, 649, 763], [-618, -824, -621], [553, 345, -567], [474, 580, 667], [-447, -329, 318], [-584, 868, -557], [544, -627, -890], [564, 392, -477], [455, 729, 728], [-892, 524, 684], [-689, 845, -530], [423, -701, 434], [7, -33, -71], [630, 319, -379], [443, 580, 662], [-789, 900, -551], [459, -707, 401]],
    [[686, 422, 578], [605, 423, 415], [515, 917, -361], [-336, 658, 858], [95, 138, 22], [-476, 619, 847], [-340, -569, -846], [567, -361, 727], [-460, 603, -452], [669, -402, 600], [729, 430, 532], [-500, -761, 534], [-322, 571, 750], [-466, -666, -811], [-429, -592, 574], [-355, 545, -477], [703, -491, -529], [-328, -685, 520], [413, 935, -424], [-391, 539, -444], [586, -435, 557], [-364, -763, -893], [807, -499, -711], [755, -354, -619], [553, 889, -390]],
    [[649, 640, 665], [682, -795, 504], [-784, 533, -524], [-644, 584, -595], [-588, -843, 648], [-30, 6, 44], [-674, 560, 763], [500, 723, -460], [609, 671, -379], [-555, -800, 653], [-675, -892, -343], [697, -426, -610], [578, 704, 681], [493, 664, -388], [-671, -858, 530], [-667, 343, 800], [571, -461, -707], [-138, -166, 112], [-889, 563, -600], [646, -828, 498], [640, 759, 510], [-630, 509, 768], [-681, -892, -333], [673, -379, -804], [-742, -814, -386], [577, -820, 562]],
    [[-589, 542, 597], [605, -692, 669], [-500, 565, -823], [-660, 373, 557], [-458, -679, -417], [-488, 449, 543], [-626, 468, -788], [338, -750, -386], [528, -832, -391], [562, -778, 733], [-938, -730, 414], [543, 643, -506], [-524, 371, -870], [407, 773, 750], [-104, 29, 83], [378, -903, -323], [-778, -728, 485], [426, 699, 580], [-438, -605, -362], [-469, -447, -387], [509, 732, 623], [647, 635, -688], [-868, -804, 481], [614, -800, 639], [595, 780, -596]],
    [[727, 592, 562], [-293, -554, 779], [441, 611, -461], [-714, 465, -776], [-743, 427, -804], [-660, -479, -426], [832, -632, 460], [927, -485, -438], [408, 393, -506], [466, 436, -512], [110, 16, 151], [-258, -428, 682], [-393, 719, 612], [-211, -452, 876], [808, -476, -593], [-575, 615, 604], [-485, 667, 467], [-680, 325, -822], [-627, -443, -432], [872, -547, -609], [833, 512, 582], [807, 604, 487], [839, -516, 451], [891, -625, 532], [-652, -548, -490], [30, -46, -14]]
  ]