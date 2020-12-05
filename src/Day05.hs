module Day05 where

import Data.List ((\\))

main = interact $ show . partTwo . lines

partOne xs = maximum $ map idNumber xs

partTwo xs = [minimum ids..maximum ids] \\ ids
  where ids = map idNumber xs

idNumber s = toDec $ map toBin s
  where toBin c | elem c "FL" = 0 | otherwise = 1
        toDec = foldl1 $ (+) . (*2)
