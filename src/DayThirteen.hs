module DayThirteen
    ( partOne
    , partTwo
    ) where

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

partOne = fileIo
    "/Users/droobertzka/Dev/advent-of-code-2020/static/InputDayThirteen.txt"
    solvePartOne


-- Part Two

type ModEq = Int -> Bool

makeEq :: (Int, [ModEq]) -> String -> (Int, [ModEq])
makeEq (i, modEqs) str =
    let
        i'          = i + 1
        n           = parse str
        shouldEqual = if i' == 0 then 0 else n - i'
    in if str == "x"
        then (i', modEqs)
        else
            (i', modEqs ++ [\x -> x `mod` n == shouldEqual])

attempt :: Int -> [ModEq] -> Int
attempt n eqs =
    if all (\eq -> eq n) eqs then n else attempt (n + 1) eqs

solvePartTwo :: Int -> [String] -> Int
solvePartTwo n raws = attempt n eqs
  where
    eqs = snd $ foldl
        makeEq
        (-1, [])
        (splitOn "," $ last raws)

partTwo =
    fileIo
            "/Users/droobertzka/Dev/advent-of-code-2020/static/InputDayThirteen.txt"
        $ solvePartTwo 100000000000000
