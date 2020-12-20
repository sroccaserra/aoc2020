module Day20 where

import Data.Char
import Text.ParserCombinators.ReadP

main = interact $ show . partOne . parse

parse s = fst $ last $ readP_to_S tiles s

labelId =
  read <$> (skipSpaces *> string "Tile " *> munch isDigit <* string ":" <* skipSpaces)
line = munch1 (`elem` ".#")
tiles = many1 (Tile <$> labelId <*> sepBy1 line (char '\n'))

partOne (a:b:_) = fitsLTR a b

data Tile = Tile Int [String]
          deriving (Show)

top (Tile _ (s:_)) = s
bottom (Tile _ (xs)) = last xs
left (Tile _ (xs)) = map head xs
right (Tile _ (xs)) = map last xs

fitsLTR a b = right a == left b

fitsTTB a b = bottom a == top b
