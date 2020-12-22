module DaySeven where

import FileIo (fileIo)
import Data.List (isInfixOf, find)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isNothing)


-- Part One

parse = splitOn " bags contain "

getQualifiedBags bags = map head . filter
    (\(_ : value : _) -> any (`isInfixOf` value) bags)

solvePartOne bagsToSearchFor qualifiedBags rules =
    let
        newQualifiedBags =
            getQualifiedBags bagsToSearchFor rules
        newRules = filter
            (\(key : _) -> key `notElem` newQualifiedBags)
            rules
        accumulatedQualifiedBags =
            qualifiedBags ++ newQualifiedBags
    in if null newQualifiedBags
        then accumulatedQualifiedBags
        else solvePartOne
            newQualifiedBags
            accumulatedQualifiedBags
            newRules

partOne =
    fileIo "static/InputDaySeven.txt"
        $ length
        . solvePartOne ["shiny gold"] []
        . map parse


-- Part Two

parseNumberFromBag bag =
    let
        unparsedNumBags = takeWhile (/= ' ') bag
        numberOfBags    = if unparsedNumBags == "no"
            then 0
            else read unparsedNumBags :: Int
    in (numberOfBags, bag)

parseContainedBags rule =
    map parseNumberFromBag $ splitOn ", " rule


partTwoParse rawRule =
    let
        (bagName : containedBags : _) =
            splitOn " bags contain " rawRule
        parsedContainedBags =
            parseContainedBags containedBags
    in (bagName, parsedContainedBags)


findRule bag = find $ (`isInfixOf` bag) . fst

type Rule = (String, [(Int, String)])

partTwoLoop :: String -> [Int] -> [Rule] -> Int
partTwoLoop bagName ns rules = if isNothing maybeRule
    then 0
    else
        let
            (bagName, containedBags) = fromJust maybeRule
            accumulator =
                product ns * sum (map fst containedBags)
            recursedNums = map
                (\(n, bagName') ->
                    partTwoLoop bagName' (n : ns) rules
                )
                containedBags
        in accumulator + sum recursedNums
    where maybeRule = findRule bagName rules

solvePartTwo rawRules = partTwoLoop
    "shiny gold"
    [1]
    parsedRules
    where parsedRules = map partTwoParse rawRules

partTwo = fileIo "static/InputDaySeven.txt" solvePartTwo
