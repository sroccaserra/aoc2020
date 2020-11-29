module Day01 where

main :: IO ()
main = do
  inputLines <- fmap lines getContents
  print $ process inputLines

process :: [String] -> [[Int]]
process = map processLine

processLine :: String -> [Int]
processLine = map read . words
