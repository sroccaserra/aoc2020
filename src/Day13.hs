module Day13 where

import Data.List.Split

main = interact $ show . partOne . parseInput . lines

parseInput :: [String] -> (Int, [Int])
parseInput (x:y:_) = (read x ::Int, map read xs)
  where xs = filter (/= "x") $ splitOn "," y
parseInput _ = error "wrong input"

partOne (n,ns) = diffs
  where diffs = map (\x -> x - (rem n x)) ns
