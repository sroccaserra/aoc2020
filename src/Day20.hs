module Day20 where

import Data.Char
import Text.ParserCombinators.ReadP
import Data.List
import Data.Maybe
import Control.Monad
import qualified Data.Vector as V

main = interact $ unlines . (map show) . partTwo . parse
-- main = do
--   s <- getContents
--   let ts = parse s
--   let res = partTwo ts
--   mapM_ (mapM_ printTile) res

printTile :: Tile -> IO ()
printTile (Tile i xs) = do
  print i
  mapM_ putStrLn xs

partOne (TileSet _ ts) = findPiecesWithNbCommons 2 bs
  where bs = map borders ts

partTwo' (TileSet _ ts) = alignSpiral [corner] (exclude corner ts)
  where bs = map borders ts
        corner = findTile ts $ head $ findPiecesWithNbCommons 2 bs

-- Plan :
-- - [X] choisir un coin, et ses deux voisins, le tourner jusqu'à ce qu'il ait
--       un voisin à droite et un voisin en bas -> c'est le coin en haut à
--       gauche (0,0).
-- - [X] itérer de (0,0) à (n,n) en cherchant les voisins sur une seule tranche
--       -> image.
-- - [X] Supplimer les coutures
-- - [ ] chercher les serpents de mer.

partTwo tileSet = solveJigsawPuzzle tileSet

solveJigsawPuzzle (TileSet r ts) = transpose image
  where bs = map borders ts
        corner = findTile ts $ head $ findPiecesWithNbCommons 2 bs
        tl = makeTopLeftCorner corner ts
        row = alignRowFromLeftTile r ts tl
        tileMap = buildTileMap row r ts
        image = buildImage tileMap

buildImage tileMap = concat $ V.toList $ V.map buildFromRow cropedTiles
  where cropedTiles = V.map (V.map cropTile) tileMap

buildFromRow r = toLines $ V.toList $ V.map toStringList r

toStringList (Tile _ xs) = xs
toLines = map concat . transpose

cropTile (Tile i xs) = Tile i (cropList $ map cropList xs)

cropList = tail . init

buildTileMap firstRow r ts = foldl (\acc _ -> buildNextRow acc r ts) (V.fromList [firstRow]) [1..r-1]

buildNextRow rows r ts = V.snoc rows newRow
  where seed = V.head $ V.last rows
        leftTile = fromJust $ findBottomMatch ts seed
        newRow = alignRowFromLeftTile r ts leftTile

alignRowFromLeftTile r ts tl = foldl (\acc _ -> alignRightMatch acc ts) (V.fromList [tl]) [1..r-1]

alignRightMatch v ts = V.snoc v rm
  where l = V.last v
        rm = fromJust $ findRightMatch ts l

makeTopLeftCorner corner ts = if isJust r && isJust b then corner else makeTopLeftCorner (rotate corner) ts
  where r = findRightMatch ts corner
        b = findBottomMatch ts corner

alignSpiral aligned [] = aligned
alignSpiral aligned ts = alignSpiral (alignedRim++aligned) (excludeTiles (map tileId rims) ts)
  where bs = map borders ts
        current = head aligned
        rims = map (findTile ts) $ findPiecesWithNbCommons 3 bs
        alignedRim = exclude current $ alignRim ([],current,rims)

alignRim (aligned, current, []) = current:aligned
alignRim (aligned,current,toAlign) = alignRim (current:aligned, next,exclude next toAlign)
  where next = fromJust $ findMatchingTile toAlign current

tileId (Tile i _) = i
exclude (Tile i _) ts = filter (\(Tile i' _) -> i' /= i) ts
excludeTiles toExclude ts = filter (\(Tile i _) -> not (elem i toExclude)) ts

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

findMatchingTiles ts t = catMaybes $ map (alignTo t) ts
findMatchingTile ts t = msum $ map (alignTo t) ts

findRightMatch ts t = msum $ map (alignRight t) ts
findBottomMatch ts t = msum $ map (alignBottom t) ts

alignTo (Tile i1 _) (Tile i2 _) | i1 == i2 = Nothing
alignTo t1 t2 = find (matches t1) (rotations ++ flipedRotations)
  where rotations = take 4 $ iterate rotate t2
        flipedRotations = take 4 $ iterate rotate (flipTile t2)

alignRight (Tile i1 _) (Tile i2 _) | i1 == i2 = Nothing
alignRight t1 t2 = find (matchesRight t1) (rotations t2 ++ flipedRotations t2)

alignBottom (Tile i1 _) (Tile i2 _) | i1 == i2 = Nothing
alignBottom t1 t2 = find (matchesBottom t1) (rotations t2 ++ flipedRotations t2)

matches t1 t2 =
  (top t1) == (bottom t2)
    || (right t1) == (left t2)
    || (bottom t1) == (top t2)
    || (left t1) == (right t2)

matchesRight t1 t2 = right t1 == left t2
matchesBottom t1 t2 = bottom t1 == top t2

top (Tile _ xs) = head xs
right (Tile _ xs) = map last xs
bottom (Tile _ xs) = last xs
left (Tile _ xs) = map head xs

rotations = take 4 . iterate rotate
rotate (Tile i xs) = Tile i (rotateLeft xs)

flipedRotations = take 4 . iterate rotate . flipTile
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
