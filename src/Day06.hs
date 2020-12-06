module Day06 where

import Data.List

main = interact $ show . partTwo . groupByEmptyLines . lines

groupByEmptyLines = filter (/= [""]) . groupBy (\x y -> x /= "" && y /= "")

partOne = sum . map countAnswers

partTwo = sum . map countValid . map stats

countAnswers :: [String] -> Int
countAnswers = length . groupBy (==) . sort . concat

stats :: [String] -> (Int, [Int])
stats group = (length group, map length $ groupBy (==) $ sort $ concat group)

countValid (x, ys) = length $ filter (== x) ys
