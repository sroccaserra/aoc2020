module Day15 where

import Data.List
import Data.List.Split
import qualified Data.Map as M
import qualified Data.Vector as V

main = interact $ show . partOne . parseLine

parseLine :: String -> [Int]
parseLine = map read . splitOn ","

partOne ns = p
  where n = 2020
        (v,m) = initMap ns
        (_,p,_) = foldl' step (m, v V.! (pred $ V.length v), V.length v) [1..n- (V.length v)]

partTwo ns = p
  where n = 30000000
        (v,m) = initMap ns
        (_,p,_) = foldl' step (m, v V.! (pred $ V.length v), V.length v) [1..n- (V.length v)]

type InitState = (V.Vector Int, M.Map Int Int)
-- m p i
type State = (M.Map Int Int, Int, Int)

initMap :: [Int] -> InitState
initMap ns@(h:_) = foldl acc (V.fromList [h], M.empty) [2..length ns]
  where acc :: InitState -> Int -> InitState
        acc (v,m) i = let n = ns !! (i-1) ; p = ns !! (i-2) in (V.snoc v n, M.insert p (i-1) m)
initMap _ = error "wrong input"

step :: State -> Int -> State
step (m,p,i) _ = (M.insert p i m, n, (succ i))
  where n = case M.lookup p m of
                 Nothing -> 0
                 Just j -> i-j
