module Day23 where

import Text.ParserCombinators.ReadP
import Data.Char

main = interact $ show . partOne . parse

parse s = fst $ last $ readP_to_S parser s

parser :: ReadP [Int]
parser = many1 $ read . (:[]) <$> (satisfy isDigit)

partOne = id
