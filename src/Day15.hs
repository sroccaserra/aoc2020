module Day15 where

import Data.List
import Data.List.Split
import qualified Data.Map as M
import qualified Data.Vector as V

main = interact $ show . partTwo . parseLine

parseLine :: String -> [Int]
parseLine = map read . splitOn ","

partOne ns = (p, i, k)
  where n = 2020
        (v,m,k) = initMap ns n
        (_,p,i,_) = foldl' step (m, v V.! (pred $ V.length v), V.length v, k) [1..n- (V.length v)]

partTwo ns = (p, i, k)
  where n = 30000000
        (v,m,k) = initMap ns n
        (_,p,i,_) = foldl' step (m, v V.! (pred $ V.length v), V.length v, k) [1..n- (V.length v)]

type InitState = (V.Vector Int, M.Map Int Int, Int)

-- m p i k
type State = (M.Map Int Int, Int, Int, Int)

initMap :: [Int] -> Int -> InitState
initMap ns@(h:_) k = foldl acc (V.fromList [h], M.empty, k) [2..length ns]
  where acc :: InitState -> Int -> InitState
        acc (v,m,k) i = let n = ns !! (i-1) ; p = ns !! (i-2) in (V.snoc v n, M.insert p (i-1) m, k)
initMap _ _ = error "wrong input"

step :: State -> Int -> State
step (m,p,i,k) _ = (M.insert p i m, n, (succ i), k)
  where n = case M.lookup p m of
                 Nothing -> 0
                 Just j -> i-j
