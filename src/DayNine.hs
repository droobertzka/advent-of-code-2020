module DayNine where

import Data.List (find)
import Data.Maybe (isNothing, fromMaybe)
import FileIo (fileIo)


-- Part One

parseNums = map (\x -> read x :: Int)

doesNotHaveSum :: [Int] -> Int -> Bool
doesNotHaveSum (n : ns) sum =
    null ns
        || (  isNothing (find (\num -> num + n == sum) ns)
           && doesNotHaveSum ns sum
           )

partOneLoop :: [Int] -> Int -> [Int] -> (Int, Int)
partOneLoop prevTwentyFiveNums i nums =
    let
        currentNum = nums !! i
        sumNotFound =
            doesNotHaveSum prevTwentyFiveNums currentNum
    in if sumNotFound
        then (currentNum, i)
        else partOneLoop
            (tail prevTwentyFiveNums ++ [currentNum])
            (i + 1)
            nums

solvePartOne prevLength nums =
    partOneLoop (take prevLength nums) prevLength nums

partOne =
    fileIo "static/InputDayNine.txt"
        $ fst
        . solvePartOne 25
        . parseNums


-- Part Two

getContiguousNums :: [Int] -> Int -> Int -> Maybe [Int]
getContiguousNums searchNums p1Answer howMany =
    let
        nums          = take howMany searchNums
        isBelowLength = length nums < howMany
        total         = sum nums
        foundSum      = total == p1Answer
    in if isBelowLength
        then Nothing
        else if foundSum
            then Just nums
            else getContiguousNums
                (tail searchNums)
                p1Answer
                howMany

partTwoLoop searchNums p1Anwser howMany = fromMaybe
    (partTwoLoop searchNums p1Anwser (howMany + 1))
    continguousNums
  where
    continguousNums =
        getContiguousNums searchNums p1Anwser howMany

solvePartTwo nums =
    let
        (p1Answer, i)   = solvePartOne 25 nums
        searchNums      = take (i - 1) nums
        continguousNums = partTwoLoop searchNums p1Answer 2
    in minimum continguousNums + maximum continguousNums

partTwo =
    fileIo "static/InputDayNine.txt"
        $ solvePartTwo
        . parseNums
