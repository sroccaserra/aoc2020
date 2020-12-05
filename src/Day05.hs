module Day05 where

import Data.List

main = interact $ show . partTwo . lines

partOne xs = maximum $ map idNumber xs

partTwo xs = [minimum ids..maximum ids] \\ ids
  where ids = map idNumber xs

idNumber s = x*8 + y
  where (a, b) = splitAt 7 s
        (x, y) = (toDec $ map toBin a, toDec $ map toBin b)
        toBin c | elem c "FL" = 0 | otherwise = 1
        toDec = foldl1' $ (+) . (*2)
