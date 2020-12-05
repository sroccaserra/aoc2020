module Day05 where

import Data.List

main = interact $ show .part2 . lines

part1 = maximum . ids

part2 xs = [minimum $ ids xs..maximum $ ids xs] \\ ids xs

ids = map $ toDec . toBin
  where toBin = map $ fromEnum . flip elem "BR"
        toDec = foldl1 $ (+) . (*2)
