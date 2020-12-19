import Data.Maybe
import Data.List
import FileIo (fileIo)

solve solver = fileIo "InputDayOne.txt" $ solver . map
    (\x -> read x :: Int)


-- Part One

findTwentyTwentySum x xs = find ((== 2020) . (+ x)) xs

partOneSolver (x : xs) =
    let match = findTwentyTwentySum x xs
    in
        if isNothing match
            then partOneSolver xs
            else x * (fromJust match)

partOne = solve partOneSolver


-- Part Two

findThreeNumbers x (y : ys) = if null ys
    then Nothing
    else
        let match = findTwentyTwentySum (x + y) ys
        in
            if isNothing match
                then findThreeNumbers x ys
                else Just (x * y * (fromJust match))

partTwoSolver (x : xs) =
    let product = findThreeNumbers x xs
    in
        if isNothing product
            then partTwoSolver xs
            else product

partTwo = solve partTwoSolver
