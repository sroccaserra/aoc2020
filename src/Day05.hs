module Day05 where

import Data.Bifunctor (bimap)
import Data.List

main = interact $ show . partTwo . (map parseLine) . lines

parseLine = bimap (toDec . map dir) (toDec .map dir) . splitAt 7
  where dir c | elem c "FL" = 0
              | otherwise = 1
        toDec = foldl1' $ (+) . (*2)

partOne xs = maximum $ map (idNumber) xs

partTwo xs = head $ [minimum ids..maximum ids] \\ ids
  where ids = map (idNumber) xs

idNumber (x, y) = x*8 + y
