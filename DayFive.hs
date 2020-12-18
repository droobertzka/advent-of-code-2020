import Data.Maybe
import Data.List
import System.IO


-- File IO

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

-- Part One

parseSeat :: Char -> [Int] -> String -> Int
parseSeat firstHalfFlag nums seat =
    let (char:restOfSeat) = seat
        half = length nums `div` 2
        remainingRows = if char == firstHalfFlag then take half nums else drop half nums
    in do
        if null restOfSeat
        then head remainingRows
        else parseSeat firstHalfFlag remainingRows restOfSeat

parseRow = parseSeat 'F' [0..127]

parseCol = parseSeat 'L' [0..7]

parseId row col = row * 8 + col

decodeSeat seat =
    let (rowCode, colCode) = splitAt 7 seat
        row = parseRow rowCode
        col = parseCol colCode
        id = parseId row col
    in id

partOne =
    fileIo "InputDayFive.txt" $ maximum . map decodeSeat

