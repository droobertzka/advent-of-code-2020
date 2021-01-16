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

makeEq offset (n, i) x = (x + i - offset) `mod` n == 0

{-
-- Debugging
makeEq offset (n, i) =
    "(x + "
        ++ show (i - offset)
        ++ ") `mod` "
        ++ show n
        ++ " == 0"
-}

makeEqs :: [(Int, Int)] -> [String] -> (Int, Int, [ModEq])
makeEqs pairs strs
    | null strs
    = let mxPair = maximum pairs
      in
          ( fst mxPair
          , snd mxPair
          , map (makeEq $ snd mxPair) pairs
          )
    | last strs == "x"
    = makeEqs pairs $ init strs
    | otherwise
    = makeEqs ((n, i) : pairs) $ init strs
    where (n, i) = (parse $ last strs, length strs - 1)

attempt :: Int -> Int -> [ModEq] -> Int
attempt first n eqs = if all (\eq -> eq n) eqs
    then n
    else attempt first (n + first) eqs

solvePartTwo :: Int -> [String] -> Int
solvePartTwo n raws =
    attempt max (n - n `mod` max) eqs - offset
  where
    (max, offset, eqs) =
        makeEqs [] (splitOn "," $ last raws)

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
