{-# LANGUAGE TupleSections #-}
module DayFourteen
    ( partOne
    , partTwo
    ) where

import Control.Monad (join)
import Data.Bits (clearBit, setBit)
import Data.List (isPrefixOf)
import Data.List.Split (splitWhen, splitOn)
import FileIo (fileIo)


-- Part One

type BitOp = (Int, Char)
type Mask = [BitOp]
type MemValue = (Int, Int) -- (Address, Value)

parseInt n = read n :: Int

isMask = isPrefixOf "mask"

parseMasks :: [String] -> [Mask]
parseMasks =
    map (zip [0 ..] . reverse . drop 7) . filter isMask

parseMemAddrNum :: String -> MemValue
parseMemAddrNum raw = (parseInt addr, parseInt n)
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

sumMemValues' :: ([Int], Int) -> MemValue -> ([Int], Int)
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


-- Part Two

applyMask2 :: Mask -> [MemValue] -> [MemValue]
applyMask2 mask = concatMap $ applyMask2' mask

applyMask2' :: Mask -> MemValue -> [MemValue]
applyMask2' mask memValue =
    snd $ foldl applyBitOp2 (memValue, [memValue]) mask

setFloatingBit :: [MemValue] -> Int -> [MemValue]
setFloatingBit memValues i =
    memValues
        >>= (\(addr, val) ->
                [ (addr `setBit` i  , val)
                , (addr `clearBit` i, val)
                ]
            )

applyBitOp2
    :: (MemValue, [MemValue])
    -> BitOp
    -> (MemValue, [MemValue])
applyBitOp2 ((addr, val), memValues) (i, char) =
    case char of
        'X' -> ((addr, val), setFloatingBit memValues i)
        '1' ->
            ( (addr, val)
            , map ((, val) . (`setBit` i) . fst) memValues
            )
        '0' -> ((addr, val), memValues)

solvePartTwo :: [String] -> Int
solvePartTwo raws = sumMemValues . reverse . join $ zipWith
    applyMask2
    (parseMasks raws)
    memAddrLists
  where
    memAddrLists =
        map (map parseMemAddrNum) . tail $ splitWhen
            isMask
            raws

partTwo = fileIo "static/InputDayFourteen.txt" solvePartTwo


-- Debugging

example1 =
    [ "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
    , "mem[8] = 11"
    , "mem[7] = 101"
    , "mem[8] = 0"
    ]

example2 =
    [ "mask = 000000000000000000000000000000X1001X"
    , "mem[42] = 100"
    , "mask = 00000000000000000000000000000000X0XX"
    , "mem[26] = 1"
    ]
