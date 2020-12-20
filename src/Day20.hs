module Day20 where

import Data.Char
import Text.ParserCombinators.ReadP

main = interact $ unlines . (map show) . partTwo . parse

parse s = TileSet side ts
  where ts = fst $ last $ readP_to_S tiles s
        side = round $ sqrt (fromIntegral $ length ts)

labelId =
  read <$> (skipSpaces *> string "Tile " *> munch isDigit <* string ":" <* skipSpaces)
line = munch1 (`elem` ".#")
tiles = many1 (Tile <$> labelId <*> sepBy1 line (char '\n'))

partOne (TileSet _ ts) = findPiecesWithNbCommons 2 bs
  where bs = map borders ts

partTwo (TileSet _ ts) = corner:findMatchingPieces rims corner
  where bs = map borders ts
        corner = head $ findPiecesWithNbCommons 2 bs
        rims = findPiecesWithNbCommons 3 bs

data TileSet = TileSet Int [Tile]
             deriving (Show)

data Tile = Tile Int [String]
          deriving (Show)

data Borders = Borders Int [String]
             deriving (Show, Eq)

findCorner = findPiecesWithNbCommons 2

findPiecesWithNbCommons n bs = map fst $ piecesWithNbcommons
  where piecesWithNbcommons = filter (\(_,n') -> n' <= n) $ map (countCommonsFromList bs) bs

findMatchingPieces bs b@(Borders _ xs) = filter (hasCommons flips) others
  where flips = xs ++ (map reverse xs)
        others = filter (/= b) bs

hasCommons fs (Borders _ xs) = [] /= filter (`elem` xs) fs

borders (Tile i xs) = Borders i [head xs,map last xs,last xs,map head xs]

countCommonsFromList bs b@(Borders _ xs) = (b,n)
  where n = foldl (\a x -> a + countCommon x flips) 0 others
        others = filter (/= b) bs
        flips = xs ++ (map reverse xs)

countCommon (Borders _ xs) fs = length $ filter (`elem` xs) fs

-- solveRims rs = findCommon start rs
--   where start = head rs

-- fitsLTR (Borders _ [_,r,_,_]) (Borders _ [_,_,_,l]) = r == l
-- fitsTTB (Borders _ [_,_,b,_]) (Borders _ [t,_,_,_]) = b == t
