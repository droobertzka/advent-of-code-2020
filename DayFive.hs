import FileIo (fileIo)
import Data.List


-- Part One

parseSeat :: Char -> [Int] -> String -> Int
parseSeat firstHalfFlag nums seat =
    let
        (char : restOfSeat) = seat
        half                = length nums `div` 2
        remainingRows       = if char == firstHalfFlag
            then take half nums
            else drop half nums
    in do
        if null restOfSeat
            then head remainingRows
            else parseSeat
                firstHalfFlag
                remainingRows
                restOfSeat

parseRow = parseSeat 'F' [0 .. 127]

parseCol = parseSeat 'L' [0 .. 7]

parseId row col = row * 8 + col

decodeSeat seat =
    let
        (rowCode, colCode) = splitAt 7 seat
        row                = parseRow rowCode
        col                = parseCol colCode
        id                 = parseId row col
    in id

partOne =
    fileIo "InputDayFive.txt" $ maximum . map decodeSeat


-- Part Two

findOurSeat possibleSeatIds actualSeatIds = find
    (\seatId ->
        notElem seatId actualSeatIds
            && elem (seatId - 1) actualSeatIds
            && elem (seatId + 1) actualSeatIds
    )
    possibleSeatIds

partTwo =
    fileIo "InputDayFive.txt"
        $ findOurSeat [0 ..]
        . map decodeSeat
