module Day03 where

import Data.Function

main :: IO ()
main = do
  inputLines <- fmap lines getContents
  print $ inputLines & partTwo

partOne :: Map -> Int
partOne m = countTrees m (3,1)

partTwo :: Map -> Int
partTwo m = foldl (*) 1 $ map (countTrees m) [(1,1), (3,1), (5,1), (7,1), (1,2)]

countTrees :: Map -> Slope -> Int
countTrees m s@(_,dy) = sum $ take n $ map (fromEnum . isTreeAt m) positions
  where positions = iterate (nextPosition s) (0,0)
        n = div 323 dy

isTreeAt :: Map -> Position -> Bool
isTreeAt m = isTree . at m
  where isTree = (== '#')

at :: Map -> Position -> Char
at m (x, y) = m !! y !! x'
  where x' = mod x w
        w = m & head & length

nextPosition :: Slope -> Position -> Position
nextPosition (dx, dy) (x, y) = (x+dx, y+dy)

type Map = [String]
type Position = (Int, Int)
type Slope = (Int, Int)
