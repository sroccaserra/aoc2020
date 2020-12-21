module Day21 where

import Data.Char
import Text.ParserCombinators.ReadP

main = interact $ unlines . (map show) . partOne . (map parseLine) . lines

parseLine = fst . last . readP_to_S parser

parser = do
  ingredients <- sepBy1 word spaces
  _ <- string " (contains "
  alergens <- sepBy1 word (string ", ")
  return (ingredients, alergens)

word = munch1 isAlpha
spaces = munch1 isSpace

partOne = id
