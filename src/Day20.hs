module Day20 where

import Data.Char
import Text.ParserCombinators.ReadP

main = interact $ show . partOne . parse

parse s = fst $ last $ readP_to_S tiles s

labelId =
  read <$> (skipSpaces *> string "Tile " *> munch isDigit <* string ":" <* skipSpaces)
line = munch1 (`elem` ".#")
tiles = many1 (Tile <$> labelId <*> sepBy1 line (char '\n'))

partOne = id

data Tile = Tile Int [String]
          deriving (Show)
