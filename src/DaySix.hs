module DaySix where

import Data.List.Split (splitWhen)
import Data.List (intercalate, nub)
import FileIo (fileIo)



solvePartOne strings = sum $ map
    (length . nub . intercalate "")
    (splitWhen (== "") strings)

partOne = fileIo "static/InputDaySix.txt" solvePartOne
