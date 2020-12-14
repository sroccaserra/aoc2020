module Day14 where

import Data.Bits
import Data.Char
import qualified Data.Map as M
import Data.List.Split
import Data.Int

main = interact $ show . partOne . parse

parse = map cutcut . chunk
  where chunk = map lines . tail . splitOn "mask = "
        cutcut (x:xs) = (parseMasks x, parseInstructions xs)
        cutcut _ = error "wrong input"

parseMasks s = (orMask s, andMask s)
  where orMask = toDec . toIntList . map (xTo '0')
        andMask = toDec . toIntList . map (xTo '1')
        toIntList :: String -> [Int64]
        toIntList = map read . tail . splitOn ""
        xTo r 'X' = r
        xTo _ c = c

parseInstructions :: [String] -> [(Int64, Int64)]
parseInstructions = map (toInstruction . words . map clean)
  where clean c | isDigit c = c
        clean _ = ' '
        toInstruction :: [String] -> (Int64, Int64)
        toInstruction (x:y:_) = (read x, read y)
        toInstruction _ = error "wrong instruction"

partOne = sum . M.elems . foldl applyInstruction initMemory

applyInstruction :: Memory ->  InstructionList -> Memory
applyInstruction m ((o,a), xs) = updateMemValues m (masked o a xs)
  where masked :: Int64 -> Int64 -> [(Int64, Int64)] -> [(Int64, Int64)]
        masked om am xs = map (\(a,v) -> (a, (om .|. v) .&. am)) xs

updateMemValues :: Memory -> [(Int64, Int64)] -> Memory
updateMemValues m xs = foldl updateMemValue m xs
  where updateMemValue :: M.Map Int64 Int64 -> (Int64, Int64) -> M.Map Int64 Int64
        updateMemValue m (a, v) = M.insert a v m 

initMemory :: Memory
initMemory = M.empty

type InstructionList = ((Int64,Int64), [(Int64,Int64)])
type Memory = M.Map Int64 Int64

toDec :: [Int64] -> Int64
toDec = foldl1 $ (+) . (*2)
