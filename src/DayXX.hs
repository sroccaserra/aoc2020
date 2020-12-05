module DayXX where

import Data.Char

-- main = interact $ show . partOne . (map parseLine) . lines
main = do
  xs <- fmap lines getContents
  mapM_ print $ map parseLine xs

parseLine = map (read::String->Int) . words . map clean
  where clean c
          | isDigit c || isAlpha c = c
          | otherwise = ' '

partOne = head
