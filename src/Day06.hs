module Day06 where

import Data.List

main = interact $ show . partTwo . lines

partOne xs = sum $ map countAnswers xs

partTwo xs = sum $ map countValid $ stats $ answersByPerson xs

countAnswers = length . groupBy (==) . sort . (filter $ not . (== ' '))

answersByPerson = map words

stats xs = map (\ group -> (length group, map length $ groupBy (==) $ sort $ foldl1 (++) group)) xs

countValid (x, ys) = length $ filter (== x) ys
