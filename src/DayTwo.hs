module DayTwo where

import Data.List
import Data.Maybe
import FileIo (fileIo)


parseLine line =
    let
        (range : charColon : pw) = words line
        dashIndex = fromJust (elemIndex '-' range)
        min = read $ take dashIndex range :: Int
        max = read $ drop (dashIndex + 1) range :: Int
    in (min, max, head charColon, head pw)

solve solver =
    fileIo "static/InputDayTwo.txt" $ solver . map parseLine


-- Part One

hasMinChars min char = (>= min) . length . filter (== char)

hasMaxChars max char = (<= max) . length . filter (== char)

isValidPartOne (min, max, char, pw) =
    let
        conditions =
            [ hasMinChars min char pw
            , hasMaxChars max char pw
            ]
    in null $ filter (== False) conditions

solvePartOne ps =
    length $ filter (== True) $ map isValidPartOne ps

partOne = solve solvePartOne


-- Part Two

hasCharAtIndex i char = (== char) . (!! (i - 1))

isValidPartTwo (i, j, char, pw) =
    let
        isAtFirstIndex  = hasCharAtIndex i char pw
        isAtSecondIndex = hasCharAtIndex j char pw
    in if isAtFirstIndex
        then not isAtSecondIndex
        else isAtSecondIndex

solvePartTwo ps =
    length $ filter (== True) $ map isValidPartTwo ps

partTwo = solve solvePartTwo
