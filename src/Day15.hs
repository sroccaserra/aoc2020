module Day15 where

import Data.List
import Data.List.Split
import qualified Data.Map as M

main = interact $ show . partTwo . parseLine

parseLine :: String -> [Int]
parseLine = map read . splitOn ","

partOne = solve 2020

partTwo = solve 30000000

solve n ns = p
  where m = initMap ns
        (_,p,_) = foldl' step (m, ns !! (pred $ length ns), length ns) [1..n-(length ns)]

type State = (M.Map Int Int, Int, Int)

initMap :: [Int] -> M.Map Int Int
initMap ns = foldl' acc M.empty [2..length ns]
  where acc m i = let p = ns !! (i-2) in M.insert p (i-1) m

step :: State -> Int -> State
step (m,p,i) _ = (M.insert p i m, n, (succ i))
  where n = case M.lookup p m of
                 Nothing -> 0
                 Just j -> i-j
