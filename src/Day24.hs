module Day24 where

import Text.ParserCombinators.ReadP

main = interact $ unlines . (map show) . partOne . (map parseLine) . lines

parseLine :: String -> [Direction]
parseLine = fst . last . readP_to_S (many1 parser)

parser :: ReadP Direction
parser = direction <$> choice [string "e", string "se", string "sw", string "w", string "nw", string "ne"]

data Direction = E | SE | SW | W | NW | NE
               deriving (Show,Eq)

direction "e" = E
direction "se" = SE
direction "sw" = SW
direction "w" = W
direction "nw" = NW
direction "ne" = NE
direction _ = error "wrong direction char"

partOne = id
