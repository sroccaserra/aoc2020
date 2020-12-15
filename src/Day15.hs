module Day15 where

-- import Debug.Trace
import Data.List.Split
import qualified Data.Map as M
import qualified Data.Vector as V

main = interact $ show . partOne . parseLine

parseLine :: String -> [Int]
parseLine = map read . splitOn ","

partOne ns = v V.! (pred $ V.length v)
  where (v,_,_) = step $ initMap ns 2020

type State = (V.Vector Int, M.Map Int Int, Int)

initMap :: [Int] -> Int -> State
initMap ns@(h:_) k = foldl acc (V.fromList [h], M.empty, k) [2..length ns]
  where acc :: State -> Int -> State
        acc (v,m,k) i = let n = ns !! (i-1) ; p = ns !! (i-2) in (V.snoc v n, M.insert p (i-1) m, k)
initMap _ _ = error "wrong input"

step :: State -> State
step st@(v,_,k) | V.length v == k = st
step (v,m,k) = step (V.snoc v n, M.insert p i m, k)
  where i = V.length v
        p = v V.! (i-1)
        n = case M.lookup p m of
                 Nothing -> 0
                 Just j -> i-j -- trace (show (m, i, j, p)) i-j
