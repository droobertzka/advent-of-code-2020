module DayThree where

import FileIo (fileIo)


getUseableIndex n str =
    let strLen = length str
    in n - (n `div` strLen * strLen)

isHash x str = str !! getUseableIndex x str == '#'

calcNumHashes (x, y) numHashes strings (slopeX, slopeY) =
    if y >= length strings
        then numHashes
        else
        -- TODO: Why is this so sensitive to indentation? Why can't the
        -- if go on the next line?
            let
                nextNumHashes = if isHash x (strings !! y)
                    then numHashes + 1
                    else numHashes
            in
                calcNumHashes
                    (x + slopeX, y + slopeY)
                    nextNumHashes
                    strings
                    (slopeX, slopeY)


-- Part 1

solvePartOne strings =
    calcNumHashes (0, 0) 0 strings (3, 1)

partOne = fileIo "static/InputDayThree.txt" solvePartOne


-- Part 2

solvePartTwo strings = product $ map
    (calcNumHashes (0, 0) 0 strings)
    [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

partTwo = fileIo "static/InputDayThree.txt" solvePartTwo
