module Day03 where

main :: IO ()
main = interact $ show . partTwo . (map cycle) . lines

partOne :: Map -> Int
partOne m = countTrees m (3,1)

partTwo :: Map -> Int
partTwo m = foldl (*) 1 $ map (countTrees m) [(1,1), (3,1), (5,1), (7,1), (1,2)]

type Map = [String]
type Position = (Int, Int)
type Slope = (Int, Int)

countTrees :: Map -> Slope -> Int
countTrees m (dx,dy) = length $ filter (== '#') $ take n $ map (at m) positions
  where n = div 323 dy
        positions = iterate nextPosition (0,0)
        nextPosition (x, y) = (x+dx, y+dy)
        at m (x, y) = m !! y !! x
