module Day24 where

import Control.Applicative
import Data.List
import Text.ParserCombinators.ReadP

data Direction = E | SE | SW | W | NW | NE
               deriving (Show,Eq)

type Point = (Int,Int,Int)

main = interact $ unlines . (map show) . partOne . (map parseLine) . lines

partOne = (:[]) . sum . map ((`mod` 2) . snd) . uniqCount . map (followPath (0,0,0))

followPath p xs = foldl move p xs

move (x,y,z) E = (x+1,y-1,z)
move (x,y,z) SE = (x,y-1,z+1)
move (x,y,z) SW = (x-1,y,z+1)
move (x,y,z) W = (x-1,y+1,z)
move (x,y,z) NW = (x,y+1,z-1)
move (x,y,z) NE = (x+1,y,z-1)

---
-- Parsing

parseLine :: String -> [Direction]
parseLine = fst . last . readP_to_S (many1 parser)

parser :: ReadP Direction
parser = direction <$> choice [string "e", string "se", string "sw", string "w", string "nw", string "ne"]

direction "e" = E
direction "se" = SE
direction "sw" = SW
direction "w" = W
direction "nw" = NW
direction "ne" = NE
direction _ = error "wrong direction char"

---
-- sort | uniq -c equivalent

uniqCount :: Ord a => [a] -> [(a, Int)]
uniqCount = map lh . sg

lh :: [a] -> (a, Int)
lh = liftA2 (,) head length

sg :: Ord a => [a] -> [[a]]
sg = group . sort
