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

factorial n = if n == 1 then n else n * factorial (n - 1)

formula :: Int -> Int
formula listLength =
    factorial listLength
        `div` (factorial (listLength - 2) * 2)

calcArrangements :: [a] -> Int
calcArrangements [_]    = 0
calcArrangements [_, _] = 2
calcArrangements xs     = formula (length xs) + 1
-- TODO: why didn't this work? Compiler says we have
-- equations with different numbers of arguments, but
-- they all take in one list.
-- calcArrangements = (+ 1) . formula . length

solvePartTwo =
    product
        . filter (/= 0)
        . map calcArrangements
        . wordsBy (/= 1)
        . getDiffs
        . sort
        . parseNums

partTwo = fileIo "static/InputDayTen.txt" solvePartTwo
