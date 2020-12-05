module Day05 where

import Data.List

main = interact $ show . partTwo . lines

partOne = maximum . idNumbers

partTwo xs = [minimum ids..maximum ids] \\ ids
  where ids = idNumbers xs

idNumbers = map $ toDec . (map toBin)
  where toBin c | elem c "FL" = 0 | otherwise = 1
        toDec = foldl1 $ (+) . (*2)
