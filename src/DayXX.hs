module DayXX where

import Data.Char
import Data.Function

main :: IO ()
main = do
  inputLines <- fmap lines getContents
  print $ inputLines & map parseLine & partOne

parseLine :: String -> [Int]
parseLine = map read . words . map clean
  where clean c
          | isDigit c || isAlpha c = c
          | otherwise = ' '

partOne :: [[Int]] -> [Int]
partOne = head
