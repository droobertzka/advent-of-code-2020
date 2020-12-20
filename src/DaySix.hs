module DaySix where

import Data.List.Split (splitWhen)
import Data.List (intercalate, intersect, nub)
import FileIo (fileIo)


-- Part One

solvePartOne strings = sum $ map
    (length . nub . intercalate "")
    (splitWhen (== "") strings)

partOne = fileIo "static/InputDaySix.txt" solvePartOne


-- Part Two

partTwo =
    fileIo "static/InputDaySix.txt"
        $ sum
        . (map (length . foldl1 intersect))
        . (splitWhen (== ""))
