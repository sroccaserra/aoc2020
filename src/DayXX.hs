module DayXX where

import Data.Char

-- main = interact $ show . partOne . (map parseLine) . lines
main = interact $ unlines . (map show) . (map parseLine) . lines

parseLine = map (read::String->Int) . words . map clean
  where clean c
          | isDigit c || isAlpha c = c
          | otherwise = ' '

partOne = id
