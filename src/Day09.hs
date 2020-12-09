module Day09 where

import qualified Data.Vector as V

main = interact $ show . partTwo . V.fromList . (map read) . lines

partOne v = findInvalidNumbers 25 v V.! 0

partTwo v = minimum ns + maximum ns
  where ns = head $ head $ filter (/= []) slices
        slices = map (\i -> findContiguousSum v n i) [0..(length v) - 1]
        n = partOne v

findInvalidNumbers n v = V.ifilter (\i _ -> not $ hasMatchingSum n v $ i+n) candidates
  where candidates = V.slice n ((length v) - n) v

previousNs :: Int -> Int -> V.Vector Int -> V.Vector Int
previousNs n i v = V.slice (i-n) n v

hasMatchingSum :: Int -> V.Vector Int -> Int -> Bool
hasMatchingSum n v i = elem (v V.! i) sums
  where ns = previousNs n i v
        sums = do
          i <- ns
          j <- ns
          return $ i + j

findContiguousSum v n i = filter ((== n) . sum) $ possibleSlices v i

possibleSlices v i = map (\n -> V.slice (i-n) (n+1) v) ns
  where ns = [0..i]
