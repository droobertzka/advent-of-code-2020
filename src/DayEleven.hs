module DayEleven where

import qualified Data.Map.Strict as Map
import FileIo (fileIo)


-- Part One

data Spot = FullSeat | EmptySeat | Floor
    deriving (Show)

type Coords = (Int, Int)

type FloorMap = Map.Map Coords Spot


convertToSpot :: Char -> Spot
convertToSpot char = case char of
    'L' -> EmptySeat
    '.' -> Floor
    '#' -> FullSeat

addRowOfEntries'
    :: (FloorMap, Coords) -> String -> (FloorMap, Coords)
addRowOfEntries' (floorMap, (col, row)) rowValue =
    let
        newMap = Map.insert
            (col, row)
            (convertToSpot (head rowValue))
            floorMap
        restOfRow = tail rowValue
    in if null restOfRow
        then (newMap, (0, 0))
        else addRowOfEntries'
            (newMap, (col + 1, row))
            restOfRow

addRowOfEntries
    :: (FloorMap, Coords) -> String -> (FloorMap, Coords)
addRowOfEntries (floorMap, (col, row)) rowValue =
    (floorMapWithNewRow, (0, row + 1))
  where
    floorMapWithNewRow = fst $ addRowOfEntries'
        (floorMap, (col, row))
        rowValue

createFloorMap :: [String] -> FloorMap
createFloorMap =
    fst . foldl addRowOfEntries (Map.empty, (0, 0))

isSpotFilled FullSeat = True
isSpotFilled _        = False

isMaybeSpotFilled = maybe False isSpotFilled

updateSeat :: Spot -> FloorMap -> Coords -> Spot
updateSeat spot floorMap (x, y) =
    let
        numAdjacentFilled = length $ filter
            (isMaybeSpotFilled . (`Map.lookup` floorMap))
            coordsToCheck
    in
        case spot of
            EmptySeat -> if numAdjacentFilled > 0
                then EmptySeat
                else FullSeat
            FullSeat -> if numAdjacentFilled > 3
                then EmptySeat
                else FullSeat
  where
    coordsToCheck =
        [ (x - 1, y - 1)
        , (x    , y - 1)
        , (x + 1, y - 1)
        , (x - 1, y)
        , (x + 1, y)
        , (x - 1, y + 1)
        , (x    , y + 1)
        , (x + 1, y + 1)
        ]

updateSpot :: FloorMap -> Coords -> Spot -> Spot
updateSpot floorMap coords spot = case spot of
    Floor -> Floor
    _     -> updateSeat spot floorMap coords

solvePartOne prevFilledSeats prevFloorMap =
    let
        nextFloorMap = Map.mapWithKey
            (updateSpot prevFloorMap)
            prevFloorMap
        filledSeats =
            length $ Map.filter isSpotFilled nextFloorMap
    in if filledSeats == prevFilledSeats
        then filledSeats
        else solvePartOne filledSeats nextFloorMap

partOne =
    fileIo "static/InputDayEleven.txt"
        $ solvePartOne 0
        . createFloorMap
