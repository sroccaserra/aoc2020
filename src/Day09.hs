module Day09 where

import qualified Data.Vector as V

main = interact $ show . partOne . V.fromList . (map read) . lines

partOne v = findInvalidNumbers 25 v V.! 0

findInvalidNumbers n v = V.ifilter (\i _ -> not $ hasMatchingSum n v $ i+n) candidates
  where candidates = V.slice n ((V.length v) - n) v

previousNs :: Int -> Int -> V.Vector Int -> V.Vector Int
previousNs n i v = V.slice (i-n) n v

hasMatchingSum :: Int -> V.Vector Int -> Int -> Bool
hasMatchingSum n v i = elem (v V.! i) sums
  where ns = previousNs n i v
        sums = do
          i <- ns
          j <- ns
          return $ i + j
