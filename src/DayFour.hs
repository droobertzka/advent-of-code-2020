module DayFour where

import qualified Data.Text as T
import qualified Data.List as L
import FileIo (fileIo)


-- Validation Data (Shared)

validateYear min max year =
    let yearAsInt = read year :: Int
    in yearAsInt >= min && yearAsInt <= max

validateBirthYr = validateYear 1920 2002
validateIssueYr = validateYear 2010 2020
validateExpirationYr = validateYear 2020 2030

validateHeight str = if L.isSuffixOf "in" str
    then
        let
            nums   = takeWhile (/= 'i') str
            height = read nums :: Int
        in height > 58 && height < 77
    else if L.isSuffixOf "cm" str
        then
            let
                nums   = takeWhile (/= 'c') str
                height = read nums :: Int
            in height > 149 && height < 194
        else False


validateHairColor (h : rest) =
    h
        == '#'
        && length rest
        == 6
        && all (`elem` "abcdef01234567890") rest

validateEyeColor =
    (`elem` [ "amb"
            , "blu"
            , "brn"
            , "gry"
            , "grn"
            , "hzl"
            , "oth"
            ]
    )

validatePassportId str =
    length str == 9 && all (`elem` "0123456789") str


requiredFields =
    ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]


-- Part 1

parsePassport acc curr = if null curr
    then acc ++ [curr]
    else (init acc) ++ [(last acc) ++ " " ++ curr]

validateField reqFields field =
    filter (/= (takeWhile (/= ':') field)) reqFields

hasRequiredFields str = (== 0) . length $ foldl
    validateField
    requiredFields
    (words str)

parseInputs :: [String] -> [String]
parseInputs = foldl parsePassport [""]

solvePartOne xs =
    length $ filter hasRequiredFields $ parseInputs xs

partOne = fileIo "static/InputDayFour.txt" solvePartOne


-- Part 2

validateFieldValue field =
    let
        [keyAsText, valueAsText] =
            T.splitOn (T.pack ":") (T.pack field)
        key   = T.unpack keyAsText
        value = T.unpack valueAsText
    in 
        -- TODO: why can't we use variables here?
        -- (when we do, the compiler warns of redundancy
        -- TODO: can we combine a let and a switch case here
        -- and then assign a validate func and then apply to `value` once?
        -- TODO: perhaps a `where` instead of the `let/in` would work better?
       case key of
        "byr" -> validateBirthYr value
        "iyr" -> validateIssueYr value
        "eyr" -> validateExpirationYr value
        "hgt" -> validateHeight value
        "hcl" -> validateHairColor value
        "ecl" -> validateEyeColor value
        "pid" -> validatePassportId value
        "cid" -> True
        _     -> False

-- TODO: this could use some refactoring.
solvePartTwo xs =
    let
        passportsWithReqFields =
            filter hasRequiredFields $ parseInputs xs
    in
        length $ filter
            (all validateFieldValue . words)
            passportsWithReqFields

partTwo = fileIo "static/InputDayFour.txt" solvePartTwo
