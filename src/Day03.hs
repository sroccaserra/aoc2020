module Day03 where

import Data.Function

main :: IO ()
main = do
  inputLines <- fmap lines getContents
  print $ inputLines & partOne

partOne :: Map -> Int
partOne m = countTrees m (3,1)

countTrees :: Map -> Slope -> Int
countTrees m s = sum $ take 323 $ map (fromEnum . isTreeAt m) positions
  where positions = iterate (nextPosition s) (0,0)

isTreeAt :: Map -> Position -> Bool
isTreeAt m = isTree . at m

isTree :: Char -> Bool
isTree = (== '#')

at :: Map -> Position -> Char
at m (x, y) = m !! y !! x'
  where x' = mod x $ width m

width :: Map -> Int
width = length . (!! 0)

nextPosition :: Slope -> Position -> Position
nextPosition (dx, dy) (x, y) = (x+dx, y+dy)

type Map = [String]
type Position = (Int, Int)
type Slope = (Int, Int)
