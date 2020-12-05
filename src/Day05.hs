module Day05 where

import Data.Bifunctor (bimap)
import Data.Char (digitToInt)
import Data.List

main = interact $ show . partTwo . (map parseLine) . lines

parseLine = bimap (map dir) (map dir) . splitAt 7
  where dir c | elem c "FL" = '0'
              | otherwise = '1'

partOne xs = maximum $ map (idNumber . seat) xs

partTwo xs = head $ [minimum ids..maximum ids] \\ ids
  where ids = map (idNumber . seat) xs

idNumber (x, y) = x*8 + y

seat = bimap toDec toDec

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0
