{-# LANGUAGE OverloadedStrings #-}

module Day06 where

import Data.List
import Data.List.Extra
import qualified Data.Text as T

main = interact $ show . partTwo . groupByEmptyLines . trimEnd

groupByEmptyLines = map (map T.unpack) . (map $ T.splitOn "\n") . (T.splitOn "\n\n") . T.pack

partOne = sum . map countAnswers

partTwo = sum . map countValid . map stats

countAnswers :: [String] -> Int
countAnswers = length . groupBy (==) . sort . concat

stats :: [String] -> (Int, [Int])
stats group = (length group, map length $ groupBy (==) $ sort $ concat group)

countValid (x, ys) = length $ filter (== x) ys
