module Day05 where

import Data.Bifunctor
import Data.List

main :: IO ()
main = interact $ show . partTwo . (map parseLine) . lines

parseLine :: String -> BoardingPass
parseLine = bimap (map dir) (map dir) . splitAt 7
  where dir c | elem c "FL" = Lower
              | otherwise = Upper

partOne :: [BoardingPass] -> Int
partOne xs = foldr max 0 $ map (idNumber . seat) xs

partTwo :: [BoardingPass] -> Int
partTwo xs = head $ [i..j] \\ ids
  where ids = map (idNumber . seat) xs
        j = foldr max 0 ids
        i = foldr min j ids

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
