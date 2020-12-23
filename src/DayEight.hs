module DayEight where

import FileIo (fileIo)


-- Part One

type Operation = (String, Int, Bool)

parse :: String -> Operation
parse raw =
    let
        plusMinusNum = drop 4 raw
        num          = if head plusMinusNum == '+'
            then drop 1 plusMinusNum
            else plusMinusNum
    in (take 3 raw, read num :: Int, False)

replaceAt xs i =
    let
        (start, end)            = splitAt i xs
        (op, num, isSecondTime) = head end
    in start ++ ((op, num, True) : tail end)

operate :: [Operation] -> Int -> Int -> Int
operate operations i acc = if isSecondTime
    then acc
    else case op of
        "nop" ->
            operate (replaceAt operations i) (i + 1) acc
        "acc" -> operate
            (replaceAt operations i)
            (i + 1)
            (acc + num)
        "jmp" ->
            operate (replaceAt operations i) (i + num) acc
    where (op, num, isSecondTime) = operations !! i

solvePartOne raws = operate (map parse raws) 0 0

partOne = fileIo "static/InputDayEight.txt" solvePartOne

