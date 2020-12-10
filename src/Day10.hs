module Day10 where

import Data.List

main = interact $ show . partOne . (map (read ::String -> Int)) . lines

partOne xs = length ones * (succ $ length threes)
  where withPlug = 0:xs
        ones = filter (== 1) $ computeDifferences withPlug
        threes = filter (== 3) $ computeDifferences withPlug

computeDifferences xs = zipWith (-) other sorted
  where sorted = sort xs
        (_:other) = sorted
