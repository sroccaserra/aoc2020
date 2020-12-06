module Day06 where

import Data.List

main = interact $ show . partOne . lines
-- main = do
--   xs <- fmap lines getContents
--   mapM_ print $ partOne xs

partOne xs = sum $ map countAnswers xs

countAnswers = length . groupBy (==) . sort
