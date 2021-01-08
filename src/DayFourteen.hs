module DayFourteen where

import Control.Monad (join)
import Data.Bits (clearBit, setBit)
import Data.List (isPrefixOf)
import Data.List.Split (splitWhen, splitOn)
import FileIo (fileIo)


-- Part One

type BitOp = (Int, Char)
type Mask = [BitOp]
type MemValue = (String, Int)

parseInt n = read n :: Int

isMask = isPrefixOf "mask"

parseMasks :: [String] -> [Mask]
parseMasks =
    map (zip [0 ..] . reverse . drop 7) . filter isMask

parseMemAddrNum :: String -> MemValue
parseMemAddrNum raw = (addr, parseInt n)
    where (addr : n : _) = splitOn "] = " $ drop 4 raw

applyMask :: Mask -> [MemValue] -> [MemValue]
applyMask mask = map $ applyMask' mask

applyMask' :: Mask -> MemValue -> MemValue
applyMask' mask memValue = foldl applyBitOp memValue mask

applyBitOp :: MemValue -> BitOp -> MemValue
applyBitOp (addr, val) (i, char) = case char of
    'X' -> (addr, val)
    '1' -> (addr, val `setBit` i)
    '0' -> (addr, val `clearBit` i)

sumMemValues :: [MemValue] -> Int
sumMemValues = snd . foldl sumMemValues' ([], 0)

sumMemValues'
    :: ([String], Int) -> MemValue -> ([String], Int)
sumMemValues' (addrs, total) (addr, n) =
    if addr `elem` addrs
        then (addrs, total)
        else (addr : addrs, total + n)

solvePartOne :: [String] -> Int
solvePartOne raws = sumMemValues . reverse . join $ zipWith
    applyMask
    (parseMasks raws)
    thing
  where
    thing = map (map parseMemAddrNum) . tail $ splitWhen
        isMask
        raws

partOne = fileIo "static/InputDayFourteen.txt" solvePartOne


-- Debugging

example =
    [ "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
    , "mem[8] = 11"
    , "mem[7] = 101"
    , "mem[8] = 0"
    ]
