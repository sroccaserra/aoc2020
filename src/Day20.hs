module Day20 where

import Data.Char
import Text.ParserCombinators.ReadP
import Data.List
import Data.Maybe

maino = interact $ unlines . (map show) . partTwo . parse
main = do
  s <- getContents
  let ts = parse s
  let res = partTwo ts
  mapM_ printTile res

printTile :: Tile -> IO ()
printTile (Tile i xs) = do
  print i
  mapM_ putStrLn xs

partOne (TileSet _ ts) = findPiecesWithNbCommons 2 bs
  where bs = map borders ts

partTwo (TileSet _ ts) = findTile ts corner : (map (findTile ts) $ findMatchingPieces rims corner)
  where bs = map borders ts
        other = head $ findMatchingPieces rims corner
        corner = head $ findPiecesWithNbCommons 2 bs
        rims = findPiecesWithNbCommons 3 bs

data TileSet = TileSet Int [Tile]
             deriving (Show)

data Tile = Tile Int [String]
          deriving (Show)

data Borders = Borders Int [String]
             deriving (Show, Eq)

findTile ts (Borders i _) = fromJust $ find (\(Tile i' _) -> i == i') ts

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

alignTo t1 t2 = find (matches t1) (rotations ++ flipedRotations)
  where rotations = take 4 $ iterate rotate t2
        flipedRotations = take 4 $ iterate rotate (flipTile t2)

matches t1 t2 =
  (top t1) == (bottom t2)
    || (right t1) == (left t2)
    || (bottom t1) == (top t2)
    || (left t1) == (right t2)

top (Tile _ xs) = head xs
right (Tile _ xs) = map last xs
bottom (Tile _ xs) = last xs
left (Tile _ xs) = map head xs

rotate (Tile i xs) = Tile i (rotateLeft xs)
flipTile (Tile i xs) = Tile i (map reverse xs)

rotateLeft :: [[a]] -> [[a]]
rotateLeft = reverse . transpose
rotateRight :: [[a]] -> [[a]]
rotateRight = transpose . reverse

-- solveRims rs = findCommon start rs
--   where start = head rs

-- fitsLTR (Borders _ [_,r,_,_]) (Borders _ [_,_,_,l]) = r == l
-- fitsTTB (Borders _ [_,_,b,_]) (Borders _ [t,_,_,_]) = b == t

parse s = TileSet side ts
  where ts = parseTiles s
        side = round $ sqrt (fromIntegral $ length ts)

parseTiles :: String -> [Tile]
parseTiles = fst . last . readP_to_S tiles

labelId = read <$> (skipSpaces *> string "Tile " *> munch isDigit <* string ":" <* skipSpaces)
line = skipSpaces *> munch1 (`elem` ".#") <* skipMany (char ' ')
tiles = many1 (Tile <$> labelId <*> sepBy1 line (char '\n'))
