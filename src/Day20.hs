module Day20 where

import Data.Char
import Text.ParserCombinators.ReadP
import Data.List
import qualified Data.Vector as V

main = interact $ show . partOne . parse

parse s = TileSet side ts
  where ts = fst $ last $ readP_to_S tiles s
        side = round $ sqrt (fromIntegral $ length ts)

labelId =
  read <$> (skipSpaces *> string "Tile " *> munch isDigit <* string ":" <* skipSpaces)
line = munch1 (`elem` ".#")
tiles = many1 (Tile <$> labelId <*> sepBy1 line (char '\n'))

partOne = find isAssembled . borderSets

data Tile = Tile Int [String]
          deriving (Show)

data TileSet = TileSet Int [Tile]
             deriving (Show)

data Borders = Borders Int String String String String
             deriving (Show)

data BorderSet = BorderSet Int (V.Vector Borders)
               deriving (Show)

borderSets (TileSet r ts) = map (fromTiles r) ps
  where ps = permutations ts

fromTile (Tile i xs@(s:_)) = Borders i s (map last xs) (last xs) (map head xs)
fromTiles r ts = BorderSet r $ V.fromList $ (map fromTile) ts
fromTileSet (TileSet r ts) = fromTiles r ts

rotate (Borders i t r b l) = Borders i (reverse l) t (reverse r) b

fitsLTR (Borders _ _ r _ _) (Borders _ _ _ _ l) = r == l
fitsTTB (Borders _ _ _ b _) (Borders _ t _ _ _) = b == t

isAssembled bs = isAssembledH bs && isAssembledV bs

isAssembledH bs@(BorderSet r _) = foldl' (\a j -> a && isRowAssembled bs j) True [0..r-2]
isRowAssembled (BorderSet r xs) j = foldl' (\a i -> a && fitsLTR (xs V.! (i+di)) (xs V.! (i+1+di))) True [0..r-2]
  where di = j*r

isAssembledV bs@(BorderSet r _) = foldl' (\a i -> a && isColumnAssembled bs i) True [0..r-2]
isColumnAssembled (BorderSet r xs) i = foldl' (\a j -> a && fitsTTB (xs V.! (j*r+i)) (xs V.! ((j+1)*r+i))) True [0..r-2]
