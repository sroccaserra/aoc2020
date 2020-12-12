module DayXX where

import Data.Char

main = interact $ unlines . (map show) . partOne . (map parseLine) . lines

parseLine = map (read::String->Int) . words . map clean
  where clean c
          | isDigit c || isAlpha c = c
          | otherwise = ' '

partOne = id
