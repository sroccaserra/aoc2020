module Day05 where

import Data.List

main = interact $ show . partTwo . lines

partOne = maximum . idNumbers

partTwo xs = [minimum ids..maximum ids] \\ ids
  where ids = idNumbers xs

idNumbers = map $ toD . (map toB)
  where toB = fromEnum . flip elem "BR"
        toD = foldl1 $ (+) . (*2)
