module DayXX where

import Data.Char

main :: IO ()
main = interact $ show . partOne . (map parseLine) . lines

parseLine :: String -> [Int]
parseLine = map read . words . map clean
  where clean c
          | isDigit c || isAlpha c = c
          | otherwise = ' '

partOne :: [[Int]] -> [Int]
partOne = head
