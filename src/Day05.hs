module Day05 where

main = interact $ show . part2 . lines

part1 = maximum . ids

part2 xs = sum [minimum $ ids xs..maximum $ ids xs] - (sum $ ids xs)

ids = map $ toDec . toBin
  where toBin = map $ fromEnum . flip elem "BR"
        toDec = foldl1 $ (+) . (*2)
