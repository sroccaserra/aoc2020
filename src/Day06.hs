module Day06 where

import Data.List

main = interact $ show . sum . map partTwo . groupByEmptyLines . lines

groupByEmptyLines = filter (/= [""]) . groupBy (\x y -> x /= "" && y /= "")

partOne :: [String] -> Int
partOne = length . nub . concat

partTwo :: [String] -> Int
partTwo = countValid . stats

stats group = (length group, map length $ groupBy (==) $ sort $ concat group)

countValid (x, ys) = length $ filter (== x) ys
