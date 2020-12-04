module Day04 where

import Data.Char
import Data.List

main :: IO ()
main = interact $ show . partTwo . (map parseLine) . lines

parseLine :: String -> Passport
parseLine s = Passport (sortedWords !! 0) (sortedWords !! 1) (sortedWords !! 2)
  (sortedWords !! 3) (sortedWords !! 4) (sortedWords !! 5) (sortedWords !! 6)
  where sortedWords = sort $ filter (not.isCid) $ words s

isCid ('c':_) = True
isCid _ = False

partTwo ps = length $ filter isValid ps

data Passport = Passport String String String String String String String
              deriving (Show)

isValid (Passport byr ecl eyr hcl hgt iyr pid)= isByrValid byr && isEclValid ecl &&
  isEyrValid eyr && isHclValid hcl && isHgtValid hgt && isIyrValid iyr && isPidValid pid

isByrValid byr = 1920 <= n && n <= 2002
  where n = read $ valueFrom byr

isEclValid ecl =
  elem ecl ["ecl:amb", "ecl:blu", "ecl:brn", "ecl:gry", "ecl:grn", "ecl:hzl", "ecl:oth"]

isEyrValid eyr = 2020 <= n && n <= 2030
  where n = read $ valueFrom eyr

isHclValid hcl = (hcl !! 4 == '#') && length s == 6 && all isHcl s
  where s = valueFrom hcl

isHcl c = isDigit c || elem c "abcdef"

isHgtValid hgt = (isSuffixOf "cm" hgt || isSuffixOf "in" hgt) && ok v u
  where u = last2 hgt
        v = read $ filter isDigit hgt ::Int
        ok n "cm" = 150 <= n && n <= 193
        ok n "in" = 59 <= n && n <= 76
        ok _ _ = False

isIyrValid iyr = 2010 <= n && n <= 2020
  where n = read $ valueFrom iyr

isPidValid pid = length s == 9 && all isDigit s
  where s = valueFrom pid

valueFrom s = last $ words $ map clean s

clean ':' = ' '
clean '#' = ' '
clean c = c

last2 xs = drop (length xs - 2) xs
