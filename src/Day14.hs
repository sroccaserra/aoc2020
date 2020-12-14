module Day14 where

import Data.Bits
import Data.Char
import qualified Data.Map as M
import Data.List.Split

main = interact $ show . partOne . parse

parse = map cutcut . chunk
  where chunk = map lines . tail . splitOn "mask = "
        cutcut (x:xs) = (x, parseInstructions xs)
        cutcut _ = error "wrong input"

parseInstructions :: [String] -> [(Int, Int)]
parseInstructions = map (toInstruction . words . map clean)
  where clean c | isDigit c = c
        clean _ = ' '
        toInstruction (x:y:_) = (read x, read y)
        toInstruction _ = error "wrong instruction"

partOne = head

toDec :: [Int] -> Int
toDec = foldl1 $ (+) . (*2)
