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
  where (_,p,_) = foldl' step (initState ns) [1..n - length ns]

type State = (M.Map Int Int, Int, Int)

initState :: [Int] -> State
initState ns = (m, p, i)
  where m = foldl' acc M.empty [1..pred $ length ns]
        acc a i = M.insert (ns !! pred i) i a
        i = length ns
        p = ns !! (pred $ length ns)

step :: State -> Int -> State
step (m,p,i) _ = (M.insert p i m, n, (succ i))
  where n = case M.lookup p m of
                 Nothing -> 0
                 Just j -> i-j
