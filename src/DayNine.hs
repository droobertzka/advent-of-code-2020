module DayNine where

import Data.List (find, findIndex)
import Data.Maybe (isNothing)
import FileIo (fileIo)


-- Part One

doesNotHaveSum :: [Int] -> Int -> Bool
doesNotHaveSum (n : ns) sum =
    null ns
        || (  isNothing (find (\num -> num + n == sum) ns)
           && doesNotHaveSum ns sum
           )

partOneLoop prevTwentyFiveNums i nums =
    let
        currentNum = nums !! i
        sumNotFound =
            doesNotHaveSum prevTwentyFiveNums currentNum
    in if sumNotFound
        then currentNum
        else partOneLoop
            (tail prevTwentyFiveNums ++ [currentNum])
            (i + 1)
            nums

solvePartOne prevLength unparsedNums = partOneLoop
    (take prevLength parsedNums)
    prevLength
    parsedNums
  where
    parsedNums = map (\x -> read x :: Int) unparsedNums

partOne =
    fileIo "static/InputDayNine.txt" $ solvePartOne 25


-- Example Data

examples =
    [ "35"
    , "20"
    , "15"
    , "25"
    , "47"
    , "40"
    , "62"
    , "55"
    , "65"
    , "95"
    , "102"
    , "117"
    , "150"
    , "182"
    , "127"
    , "219"
    , "299"
    , "277"
    , "309"
    , "576"
    ]
