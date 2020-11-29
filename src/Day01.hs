module Day01 where

main = do
    lines <- fmap lines getContents
    print $ process lines

process :: [String] -> [[Int]]
process = map processLine

processLine :: String -> [Int]
processLine = map read . words
