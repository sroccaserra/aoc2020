module Day05 where

import Data.Bifunctor

main :: IO ()
main = interact $ show . partOne . (map parseLine) . lines

parseLine :: String -> BoardingPass
parseLine s = bimap f f xs
  where xs = span (flip elem "FB") s
        f = map fblrToUpOrDown

fblrToUpOrDown :: Char -> Direction
fblrToUpOrDown 'F' = Lower
fblrToUpOrDown 'B' = Upper
fblrToUpOrDown 'L' = Lower
fblrToUpOrDown _ = Upper

partOne :: [BoardingPass] -> Int
partOne xs = foldr max 0 $ map idNumber seats
  where seats = map (bimap findRow findColumn) xs
        findRow = bisectBy 0 127
        findColumn = bisectBy 0 7
        idNumber (x, y) = x * 8 + y

type BoardingPass = ([Direction], [Direction])
data Direction = Lower | Upper
               deriving (Show)

-- 0  127 L -> 0 63   -> x      , (y+1)/2-1
-- 0  127 U -> 64 127 -> (y+1)/2, y
-- 0  63  U -> 32 63  -> (y+1)/2, y
-- 32 63  L -> 32 47  -> x      , x+(y+1-x)/2-1
bisectBy :: Int -> Int -> [Direction] -> Int
bisectBy x _ [] = x
bisectBy x y (Upper:rest) = bisectBy (mid x y) y rest
bisectBy x y (Lower:rest) = bisectBy x ((mid x y) -1) rest

mid :: Int -> Int -> Int
mid x y = x + (div (y+1-x) 2)
