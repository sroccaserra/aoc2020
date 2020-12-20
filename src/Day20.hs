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

partOne = fromTileSet

data Tile = Tile Int [String]
          deriving (Show)

data TileSet = TileSet Int [Tile]
             deriving (Show)

data Borders = Borders Int String String String String
             deriving (Show)

data BorderSet = BorderSet Int (V.Vector Borders)
               deriving (Show)

fromTile (Tile i xs@(s:_)) = Borders i s (map last xs) (last xs) (map head xs)
fromTileSet (TileSet r ts) = BorderSet r (V.fromList $ map fromTile ts)

rotate (Borders i t r b l) = Borders i (reverse l) t (reverse r) b

fitsLTR (Borders _ _ r _ _) (Borders _ _ _ _ l) = r == l
fitsTTB (Borders _ _ _ b _) (Borders _ t _ _ _) = b == t
