module Day06 where

import Data.List

main = interact $ show . partTwo . groupByEmptyLines

groupByEmptyLines = filter (/= [""]) . groupBy (\x y -> x /= "" && y /= "") . lines

partOne :: [[String]] -> Int
partOne = sum . (map $ length . foldl1 union)

partTwo :: [[String]] -> Int
partTwo = sum . (map $ length . foldl1 intersect)
