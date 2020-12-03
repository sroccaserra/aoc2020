module Day03 where

import Data.Function

main :: IO ()
main = do
  inputLines <- fmap lines getContents
  print $ inputLines & partOne

partOne :: Map -> Int
partOne m = sum $ take 323 $ map (fromEnum . isTreeAt m) positions
  where positions = iterate nextPosition (0,0)

isTreeAt :: Map -> Position -> Bool
isTreeAt m = isTree . at m

isTree :: Char -> Bool
isTree = (== '#')

at :: Map -> Position -> Char
at m (x, y) = m !! y !! x'
  where x' = mod x $ width m

width :: Map -> Int
width = length . (!! 0)

nextPosition :: Position -> Position
nextPosition (x, y) = (x+3, y+1)

type Map = [String]
type Position = (Int, Int)
