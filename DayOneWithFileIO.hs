import Data.Maybe
import Data.List
import System.IO

fileIo filename findAnswer = do
    inh <- openFile filename ReadMode
    mainloop inh ([] :: [String]) findAnswer
    hClose inh

mainloop inh list findAnswer = do
    isEOF <- hIsEOF inh
    if isEOF
        then do
            print $ findAnswer list
        else do
            nextLineIn <- hGetLine inh
            mainloop inh (list ++ [nextLineIn]) findAnswer

findTwentyTwentySum x xs =
    find ((== 2020) . (+ x)) xs

partOne (x:xs) =
    let match = findTwentyTwentySum x xs
    in
        if
            isNothing match
        then
            partOne xs
        else
            x * (fromJust match)

main = do
    fileIo "day-1.txt"  $ partOne . map (\x -> read x :: Int)


