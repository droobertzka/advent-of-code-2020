module DaySeven where

import FileIo (fileIo)
import Data.List (isInfixOf)
import Data.List.Split (split, onSublist, dropDelims)


-- Part One

parse = split (dropDelims $ onSublist " bags contain ")

-- canContain bag value = bag `isInfixOf` value 

reducer bag bagsThatQualify (key : value : _) =
    if bag `isInfixOf` value
        then key : bagsThatQualify
        else bagsThatQualify

solvePartOne bag bagsThatQualify rules = if null rules
    then bagsThatQualify
    else
        let
            newBagsThatQualify =
                foldl (reducer bag) bagsThatQualify rules
            newRules = filter
                (\(key : _) ->
                    key `notElem` newBagsThatQualify
                )
                rules
        in solvePartOne
            (head newBagsThatQualify)
            newBagsThatQualify
            newRules

partOne =
    fileIo "static/InputDaySeven.txt"
        $ solvePartOne "shiny gold" []
        . map parse
