module DayXX where

import Data.Function

main :: IO ()
main = do
  inputLines <- fmap lines getContents
  print $ inputLines & parse & partOne

parse :: [String] -> [Int]
parse = map read

partOne :: [Int] -> Int
partOne = head
