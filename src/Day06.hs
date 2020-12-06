module Day06 where

import Data.List
import Data.Bifunctor

main = interact $ show . bimap partOne partTwo . dup . groupByEmptyLines
  where dup x = (x,x)

groupByEmptyLines = filter (/= [""]) . groupBy (\x y -> x /= "" && y /= "") . lines

partOne :: [[String]] -> Int
partOne = sum . (map $ length . foldl1 union)

partTwo :: [[String]] -> Int
partTwo = sum . (map $ length . foldl1 intersect)
