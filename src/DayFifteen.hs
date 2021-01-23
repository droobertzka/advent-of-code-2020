module DayFifteen
    ( partTwo
    ) where

import Data.List (find)
import Data.Maybe (maybe, isNothing, isJust, fromJust)
import qualified Data.Map as Map


-- Part One

addNextPair :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
addNextPair (n, i) pairs = maybe
    (pairs ++ [(0, i + 1)])
    (\match -> pairs ++ [(i - snd match, i + 1)])
    maybeMatch
  where
    maybeMatch = find ((== n) . fst) $ reverse $ init pairs

partOneLoop :: [(Int, Int)] -> Int
partOneLoop pairs
    | snd lastPair == 2020 = fst lastPair
    | otherwise = partOneLoop $ addNextPair lastPair pairs
    where lastPair = last pairs

partOne :: [Int] -> Int
partOne ns = partOneLoop $ zip ns [1 ..]

inputs :: [Int]
inputs = [1, 0, 16, 5, 17, 4]


-- Part Two

type SpokenNums = Map.Map Int Int

partTwoLoop :: Int -> SpokenNums -> Int -> Int -> Int
partTwoLoop end m prevSpoken counter
    | counter == end = prevSpoken
    | otherwise = partTwoLoop
        end
        (Map.insert prevSpoken counter m)
        (maybe 0 (counter -) (Map.lookup prevSpoken m))
        (succ counter)

partTwo' :: [Int] -> Int -> Int
partTwo' ns countTo = partTwoLoop
    countTo
    map
    0
    (length ns + 1)
    where map = Map.fromList $ zip ns [1 ..]

partTwo n = do
    print $ partTwo' inputs n


-- Debugging

examples :: [Int]
examples = [0, 3, 6]
