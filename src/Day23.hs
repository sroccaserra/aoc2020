module Day23 where

import Data.Char
import Data.List
import Data.Maybe
import Text.ParserCombinators.ReadP

main = interact $ show . partOne . parse

parse s = fst $ last $ readP_to_S parser s

parser :: ReadP [Int]
parser = many1 $ read . (:[]) <$> (satisfy isDigit)

partOne = concatMap show . tail . alignToOne . last . take 101 . iterate step

step (current:rest) = tail (concat [a, (take 3 rest), c]) ++ [current]
  where subList = current : (drop 3 rest)
        destIndex = getDestIndex current subList
        (a, c) = splitAt (succ destIndex) subList
step _ = error "list is too small?"

getDestIndex :: Int -> [Int] -> Int
getDestIndex n subList =
  if elem i' subList then fromJust $ elemIndex i' subList else getDestIndex i' subList
  where i = n - 1
        i' = if i == 0 then 9 else i

alignToOne xs = end ++ begin
  where i = fromJust $ elemIndex 1 xs
        (begin, end) = splitAt i xs
