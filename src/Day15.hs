module Day15 where

-- import Debug.Trace
import Data.List.Split
import qualified Data.Map as M
import qualified Data.Vector as V

main = interact $ show . partOne . parseLine

parseLine :: String -> [Int]
parseLine = map read . splitOn ","

partOne ns = (p, i, k)
  where (v,m,k) = initMap ns 2020
        (_,p,i,_) = step (m, v V.! (pred $ V.length v), V.length v, k)

type InitState = (V.Vector Int, M.Map Int Int, Int)

-- m p i k
type State = (M.Map Int Int, Int, Int, Int)

initMap :: [Int] -> Int -> InitState
initMap ns@(h:_) k = foldl acc (V.fromList [h], M.empty, k) [2..length ns]
  where acc :: InitState -> Int -> InitState
        acc (v,m,k) i = let n = ns !! (i-1) ; p = ns !! (i-2) in (V.snoc v n, M.insert p (i-1) m, k)
initMap _ _ = error "wrong input"

step :: State -> State
step st@(_,_,i,k) | i == k = st
step (m,p,i,k) = step (M.insert p i m, n, (succ i), k)
  where n = case M.lookup p m of
                 Nothing -> 0
                 Just j -> i-j
-- V.length v -> i
-- v V.! (i-1) -> p
--
