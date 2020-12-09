module Day09 where

import qualified Data.Vector as V

main = interact $ show . partOne . V.fromList . (map parseLine) . lines

parseLine :: String -> Int
parseLine = read

partOne v = V.ifilter (\i _ -> not $ hasMatchingSum 25 v $ i+25) candidates
  where candidates = V.slice 25 ((V.length v) - 25) v

previousNs :: Int -> Int -> V.Vector Int -> V.Vector Int
previousNs n i v = V.slice (i-n) n v

generateIndexes n v = [n..pred $ V.length v]

hasMatchingSum :: Int -> V.Vector Int -> Int -> Bool
hasMatchingSum n v i = elem (v V.! i) sums
  where ns = previousNs n i v
        sums = do
          i <- ns
          j <- ns
          return $ i + j
