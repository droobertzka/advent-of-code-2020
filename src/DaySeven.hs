module DaySeven where

import FileIo (fileIo)
import Data.List (isInfixOf)
import Data.List.Split (split, onSublist, dropDelims)


-- Part One

parse = split (dropDelims $ onSublist " bags contain ")

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

-- Our Proposed Logic:
-- 1. parse the list of strings into ["bag", "rule rule rule"] parts
-- 2. loop over the parsed rules, look for the given bag, and make a list of
--    bags in the rules that can contain it (aka, they qualify)
-- 3. make a new list of rules that doesn't contain any bags that are already
--    in our list of bags that qualify
-- 4. with the new list of bags and the new (shortened) list of rules, find all
--    the bags that can contain any of those bags
--
--    PROBLEM in setp 2, we needed to keep a running list
--    of all the bags that have qualified, but we were just passing in the
--    next set of bags we found that can contain the current bag. This
--    was causing an infinite loop because every bag can be contained in
--    at least one other bag.
--
--    PROBLEM 2: we weren't differentiating bewteen bags we wanted to search
--    for and the list of qualified bags. 
--
--    PROBLEM 3: we weren't concatenating the qualified bags list each time, so
--    we ended up with a list with a length of less than 6.
--
--    NOTE: we were folding with our `reduce` function, but it was easier to
--    get the logic right with map + reeduce.
