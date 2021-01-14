module DayThirteen
    ( partOne
    , partTwo
    ) where

import Debug.Trace
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

getRemainder n i
    | i == 0    = 0
    | n - i > 0 = n - i
    | otherwise = n - (i `mod` n)

makeEq :: (Int, [ModEq]) -> String -> (Int, [ModEq])
makeEq (i, modEqs) str =
    let
        i'    = i + 1
        n     = parse str
        modEq = \x -> x `mod` n == getRemainder n i'
    in if str == "x"
        then (i', modEqs)
        else (i', modEqs ++ [modEq])


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


-- Part Two (using Chinese Remainder Theorem)

findX n modNum i = if i * n `mod` modNum == 1
    then i
    else findX n modNum (i + 1)

makeRnX :: Int -> (Int, Int) -> Int
makeRnX prod (c, modNum) =
    let
        r = modNum - c
        prodOtherMods = prod `div` modNum
        x = findX (prodOtherMods `mod` modNum) modNum 1
    in r * prodOtherMods * x

crt :: Int -> [(Int, Int)] -> Int
crt prod pairs = total `mod` prod
    where total = sum $ map (makeRnX prod) pairs

makePairs
    :: (Int, [(Int, Int)]) -> String -> (Int, [(Int, Int)])
makePairs (i, pairs) str = if str == "x"
    then (i', pairs)
    else (i', pairs ++ [(i', parse str)])
    where i' = i + 1

solvePartTwo' :: [String] -> Int
solvePartTwo' raws = crt (product $ map snd pairs) pairs
  where
    pairs = snd $ foldl
        makePairs
        (-1, [])
        (splitOn "," $ last raws)

partTwo' = fileIo
    "/Users/droobertzka/Dev/advent-of-code-2020/static/InputDayThirteen.txt"
    solvePartTwo'
