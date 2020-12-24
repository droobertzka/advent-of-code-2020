module DayEight where

import FileIo (fileIo)
import Data.List (findIndices)


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

operate :: [Operation] -> Int -> Int -> (Int, Bool)
operate operations i acc = if isSecondTime
    then (acc, False)
    else case op of
        "nop" ->
            operate (replaceAt operations i) (i + 1) acc
        "acc" -> operate
            (replaceAt operations i)
            (i + 1)
            (acc + num)
        "jmp" ->
            operate (replaceAt operations i) (i + num) acc
        "end" -> (acc, True)
    where (op, num, isSecondTime) = operations !! i

solvePartOne raws = fst $ operate (map parse raws) 0 0

partOne = fileIo "static/InputDayEight.txt" solvePartOne


-- Part Two

replaceElement i newEl xs = start ++ newEl : tail end
    where (start, end) = splitAt i xs

flipOp i ops =
    let
        (op, num, bool) = ops !! i
        newOp = if op == "nop" then "jmp" else "nop"
    in replaceElement i (newOp, num, bool) ops

partTwoLoop :: [Operation] -> [Int] -> Int
partTwoLoop ops (i : tailIndices) =
    let
        updatedOps         = flipOp i ops
        (acc, didComplete) = operate updatedOps 0 0
    in if didComplete
        then acc
        else partTwoLoop ops tailIndices

solvePartTwo raws = partTwoLoop
    parsedOps
    (findIndices
        (\(x, _, _) -> x == "nop" || x == "jmp")
        parsedOps
    )
    where parsedOps = map parse raws ++ [("end", 0, False)]

partTwo = fileIo "static/InputDayEight.txt" solvePartTwo
