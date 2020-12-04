module Day04 where

import Data.Char
import Data.List

main = interact $ show . partTwo . (map parseLine) . lines

parseLine :: String -> Passport
parseLine s = Passport (read $ values !! 0) (values !! 1) (read $ values !! 2)
  (values !! 3) (values !! 4) (read $ values !! 5) (values !! 6)
  where sortedWords = sort $ filter (not.isCid) $ words s
        values = map (snd . splitAt 4) sortedWords

isCid ('c':_) = True
isCid _ = False

partTwo ps = length $ filter isValid ps

data Passport = Passport Int String Int String String Int String
              deriving (Show)

isValid (Passport byr ecl eyr hcl hgt iyr pid) =
  isByrValid byr && isEclValid ecl && isEyrValid eyr && isHclValid hcl &&
  isHgtValid hgt && isIyrValid iyr && isPidValid pid

isByrValid n = 1920 <= n && n <= 2002
isEyrValid n = 2020 <= n && n <= 2030
isIyrValid n = 2010 <= n && n <= 2020

isEclValid ecl = elem ecl ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

isHclValid ('#':s) = length s == 6 && all isHexDigit s
isHclValid _ = False

isHgtValid hgt = elem u ["cm", "in"] && ok v u
  where u = last2 hgt
        v = read $ filter isDigit hgt
        ok n "cm" = 150 <= n && n <= 193
        ok n "in" = 59 <= n && n <= 76
        ok _ _ = False

isPidValid s = length s == 9 && all isDigit s

last2 xs = drop (length xs - 2) xs
