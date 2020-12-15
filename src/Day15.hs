module Day15 where

import Data.List
import Data.List.Split

main = interact $ show . partOne . parseLine

parseLine :: String -> [Int]
parseLine = reverse . map read . splitOn ","

partOne ns = head $ (iterate step ns) !! (2020-length ns)

step :: [Int] -> [Int]
step ns@(n:rest) = next:ns
  where next = case elemIndex n rest of
                    Nothing -> 0
                    Just i -> succ i
step _ = error "wrong input"
