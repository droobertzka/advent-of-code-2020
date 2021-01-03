module DayThirteen where

import Data.List.Split (splitOn)
import Data.List
import Data.Maybe (fromJust)
import FileIo (fileIo)


-- Part One

parse n = read n :: Int

findDepartureDiff :: Int -> Int -> Int
findDepartureDiff earliest busNum =
    busNum - (earliest `mod` busNum)

solvePartOne :: [String] -> Int
solvePartOne raws =
    let
        earliestDeparture = parse (head raws)
        buses =
            map parse $ filter (/= "x") $ splitOn "," $ last
                raws
        diffs =
            map (findDepartureDiff earliestDeparture) buses
        index = fromJust $ elemIndex (minimum diffs) diffs
    in (diffs !! index) * (buses !! index)

partOne = fileIo "static/InputDayThirteen.txt" solvePartOne


-- Debugging

example = ["939", "7,13,x,x,59,x,31,19"]
