module DaySixteen where

import Data.List.Split (splitOn, splitWhen)
import FileIo (fileIo)


-- Part One

parseInt x = read x :: Int

getRanges :: String -> [String]
getRanges = splitOn " or " . (!! 1) . splitOn ": "

parseRule :: String -> [Int]
parseRule =
    map parseInt . concatMap (splitOn "-") . getRanges

applyRule :: Int -> [Int] -> Bool
applyRule n (a : b : c : d : _) =
    n > a && n < b || n > c && n < d

noRulesPass :: [[Int]] -> Int -> Bool
noRulesPass rules n = not $ any (applyRule n) rules

parseTickets :: [String] -> [Int]
parseTickets = concatMap (map parseInt . splitOn ",")

solvePartOne :: [[String]] -> Int
solvePartOne (rules : _ : ts : _) = sum $ filter
    (noRulesPass parsedRules)
    (parseTickets $ tail ts)
    where parsedRules = map parseRule rules

partOne =
    fileIo "static/InputDaySixteen.txt"
        $ solvePartOne
        . splitWhen null
