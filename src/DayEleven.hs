module DayEleven where

import qualified Data.Map.Strict as Map
import Data.Bifunctor (bimap)
import Data.Maybe (isNothing)
import FileIo (fileIo)


-- Types

data Spot = FullSeat | EmptySeat | Floor
    deriving (Show, Eq)

type Coords = (Int, Int)

type FloorMap = Map.Map Coords Spot

type SpotsGetter = Coords -> FloorMap -> [Maybe Spot]


-- Part One

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

updateSeat
    :: Int -> Int -> Spot -> FloorMap -> Coords -> Spot
updateSeat numAdjacentFilled numSeatsToEmpty spot floorMap (x, y)
    = case spot of
        EmptySeat -> if numAdjacentFilled > 0
            then EmptySeat
            else FullSeat
        FullSeat -> if numAdjacentFilled >= numSeatsToEmpty
            then EmptySeat
            else FullSeat

updateSpot
    :: SpotsGetter
    -> Int
    -> FloorMap
    -> Coords
    -> Spot
    -> Spot
updateSpot spotsGetter numSeatsToEmpty floorMap (x, y) spot
    = case spot of
        Floor -> Floor
        _     -> updateSeat
            numAdjacentFilled
            numSeatsToEmpty
            spot
            floorMap
            (x, y)
  where
    numAdjacentFilled =
        length $ filter isMaybeSpotFilled $ spotsGetter
            (x, y)
            floorMap

deltas :: [Coords]
deltas =
    [ (-1, -1)
    , (0 , -1)
    , (1 , -1)
    , (-1, 0)
    , (1 , 0)
    , (-1, 1)
    , (0 , 1)
    , (1 , 1)
    ]

getSpotsToCheck1 :: SpotsGetter
getSpotsToCheck1 (x, y) floorMap =
    map ((`Map.lookup` floorMap) . bimap (+ x) (+ y)) deltas

solve :: SpotsGetter -> Int -> Int -> FloorMap -> Int
solve spotsGetter numSeatsToEmpty prevFilledSeats prevFloorMap
    = let
          nextFloorMap = Map.mapWithKey
              (updateSpot
                  spotsGetter
                  numSeatsToEmpty
                  prevFloorMap
              )
              prevFloorMap
          filledSeats =
              length $ Map.filter isSpotFilled nextFloorMap
      in if filledSeats == prevFilledSeats
          then filledSeats
          else solve
              spotsGetter
              numSeatsToEmpty
              filledSeats
              nextFloorMap

solvePartOne = solve getSpotsToCheck1 4 0

partOne =
    fileIo "static/InputDayEleven.txt"
        $ solvePartOne
        . createFloorMap


-- Part Two

getSeatInDirection
    :: Coords -> FloorMap -> Coords -> Maybe Spot
getSeatInDirection (x, y) floorMap delta =
    let
        spotToCheck = bimap (+ x) (+ y) delta
        maybeSpot   = Map.lookup spotToCheck floorMap
    in case maybeSpot of
        Nothing -> maybeSpot
        Just Floor ->
            getSeatInDirection spotToCheck floorMap delta
        Just _ -> maybeSpot

getSpotsToCheck2 :: SpotsGetter
getSpotsToCheck2 coords floorMap =
    map (getSeatInDirection coords floorMap) deltas

solvePartTwo = solve getSpotsToCheck2 5 0

partTwo =
    fileIo "static/InputDayEleven.txt"
        $ solvePartTwo
        . createFloorMap
