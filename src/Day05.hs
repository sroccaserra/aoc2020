module Day05 where

import Data.Bifunctor
import Data.List

main :: IO ()
main = interact $ show . partTwo . (map parseLine) . lines

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
partOne xs = foldr max 0 $ map (idNumber . seat) xs

partTwo :: [BoardingPass] -> Int
partTwo xs = head $ [28..842] \\ ids
  where ids = map (idNumber . seat) xs

type Seat = (Int, Int)
type BoardingPass = ([Direction], [Direction])
data Direction = Lower | Upper
               deriving (Show)

idNumber :: Seat -> Int
idNumber (x, y) = x*8 + y

seat :: BoardingPass -> Seat
seat bp = bimap findRow findColumn bp
  where findRow = bisectBy 0 127
        findColumn = bisectBy 0 7

bisectBy :: Int -> Int -> [Direction] -> Int
bisectBy x _ [] = x
bisectBy x y (Upper:rest) = bisectBy (mid x y) y rest
bisectBy x y (Lower:rest) = bisectBy x ((mid x y) -1) rest

mid :: Int -> Int -> Int
mid x y = x + (div (y+1-x) 2)
