module DayTen where

import Data.List (sort)
import Data.List.Split (wordsBy)
import FileIo (fileIo)


-- Part One

parseNums = map (\x -> read x :: Int)

incDiffCount (diffOneCount, diffThreeCount) diff =
    if diff == 1
        then (diffOneCount + 1, diffThreeCount)
        else (diffOneCount, diffThreeCount + 1)

partOneLoop
    :: (Int, (Int, Int)) -> Int -> (Int, (Int, Int))
partOneLoop (prevNum, diffCounts) currNum = if prevNum == 0
    then (currNum, (0, 0))
    else (currNum, incDiffCount diffCounts diff)
    where diff = currNum - prevNum

solvePartOne ns = (diffOneCount + 1) * (diffThreeCount + 1)
  where
    (_, (diffOneCount, diffThreeCount)) =
        foldl partOneLoop (0, (0, 0)) ns

partOne =
    fileIo "static/InputDayTen.txt"
        $ solvePartOne
        . sort
        . parseNums


-- Part Two

getDiffsLoop :: (Int, [Int]) -> Int -> (Int, [Int])
getDiffsLoop (prevNum, diffs) currNum = if prevNum == -1
    then (currNum, diffs)
    else (currNum, diffs ++ [currNum - prevNum])

getDiffs :: [Int] -> [Int]
getDiffs ns =
    snd
        $  foldl getDiffsLoop (-1, [])
        $  [0]
        ++ ns
        ++ [lastNum]
    where lastNum = maximum ns + 3

formula n = formula' (n - 1) [4, 2, 1]

formula' :: Int -> [Int] -> Int
formula' count currentList
    | count == 0
    = last currentList
    | otherwise
    = formula' (count - 1) $ next : init currentList
    where next = sum currentList

calcArrangements :: [a] -> Int
calcArrangements xs = formula $ length xs

solvePartTwo =
    product
        . map calcArrangements
        . wordsBy (/= 1)
        . getDiffs
        . sort
        . parseNums

partTwo = fileIo "static/InputDayTen.txt" solvePartTwo
