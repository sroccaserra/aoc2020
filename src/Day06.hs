module Day06 where

import Data.List

main = interact $ show . partTwo . lines

partOne = sum . map countAnswers

partTwo = sum . map countValid . map stats . answersByPerson

countAnswers = length . groupBy (==) . sort . (filter $ not . (== ' '))

answersByPerson = map words

stats group = (length group, map length $ groupBy (==) $ sort $ foldl1 (++) group)

countValid (x, ys) = length $ filter (== x) ys
