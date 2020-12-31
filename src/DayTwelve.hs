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
