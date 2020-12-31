module DayTwelve where

import FileIo (fileIo)


-- Part One

type Instruction = (Char, Int)
type Coords = (Int, Int)

parse :: String -> Instruction
parse raw = (head raw, read (tail raw) :: Int)

moveForward :: Coords -> Int -> Int -> Coords
moveForward (x, y) n 90  = (x, y + n)
moveForward (x, y) n 0   = (x + n, y)
moveForward (x, y) n 180 = (x - n, y)
moveForward (x, y) n 270 = (x, y - n)

turn :: Char -> Int -> Int -> Int
turn leftOrRight degrees currentDirection =
    let
        delta = if leftOrRight == 'L'
            then degrees
            else 360 - degrees
        newDirection = currentDirection + delta
    in if newDirection >= 360
        then newDirection `mod` 360
        else newDirection

applyInstruction
    :: (Coords, Int) -> String -> (Coords, Int)
applyInstruction ((x, y), direction) rawInstruction =
    case action of
        'N' -> ((x, y + n), direction)
        'E' -> ((x + n, y), direction)
        'W' -> ((x - n, y), direction)
        'S' -> ((x, y - n), direction)
        'F' -> (moveForward (x, y) n direction, direction)
        _   -> ((x, y), turn action n direction)
    where (action, n) = parse rawInstruction

solvePartOne :: [String] -> Int
solvePartOne raws = abs x + abs y
  where
    ((x, y), _) = foldl applyInstruction ((0, 0), 0) raws

partOne = fileIo "static/InputDayTwelve.txt" solvePartOne


-- Part Two

data Position = Position
    { shipCoords     :: Coords
    , waypointCoords :: Coords
    }

moveToWaypoint :: Position -> Position
moveToWaypoint (Position shipCoords wpCoords) = Position
    (newX, newY)
    wpCoords
  where
    [newX, newY] = uncurry (+) `map` [shipCoords, wpCoords]

turnWaypoint :: Int -> Coords -> Coords
turnWaypoint degrees (wpx, wpy) = case degrees of
    90  -> (-wpy, wpx)
    180 -> (-wpx, -wpy)
    270 -> (wpy, -wpx)

turn2 :: Position -> Char -> Int -> Position
turn2 (Position shipCoords wpCoords) action degrees =
    Position
        shipCoords
        (turnWaypoint translatedDeg wpCoords)
  where
    translatedDeg =
        if action == 'L' then degrees else 360 - degrees

applyInstruction2 :: Position -> String -> Position
applyInstruction2 (Position (x, y) (wpx, wpy)) rawInstruction
    = case action of
        'N' -> Position (x, y) (wpx, wpy + n)
        'E' -> Position (x, y) (wpx + n, wpy)
        'W' -> Position (x, y) (wpx - n, wpy)
        'S' -> Position (x, y) (wpx, wpy - n)
        'F' ->
            iterate
                    moveToWaypoint
                    (Position (x, y) (wpx, wpy))
                !! n
        _ -> turn2 (Position (x, y) (wpx, wpy)) action n
    where (action, n) = parse rawInstruction

solvePartTwo :: [String] -> Int
solvePartTwo raws = abs x + abs y
  where
    (Position (x, y) _) = foldl
        applyInstruction2
        (Position (0, 0) (10, 1))
        raws

partTwo = fileIo "static/InputDayTwelve.txt" solvePartTwo
