module DayTen where

import Data.List (sort)
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
