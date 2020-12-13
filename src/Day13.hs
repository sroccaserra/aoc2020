module Day13 where

import Data.Int
import Data.List.Split
import Debug.Trace

main = interact $ show . partTwo . (map $ buildInput . words) . lines

parseInput :: [String] -> (Int, [Int])
parseInput (x:y:_) = (read x ::Int, map read xs)
  where xs = filter (/= "x") $ splitOn "," y
parseInput _ = error "wrong input"

partOne (n,ns) = diffs
  where diffs = map (\x -> x - (rem n x)) ns

buildInput (x:y:n:[]) = (read x, read y,read n)
buildInput _ = error "wrong input"

partTwo :: [(Int64,Int64,Int64)] -> Int64
partTwo = findFirstCommon . (map values)

f x y n = f' x x y y n

f' _ a _ b n | a + n == b = a
f' x a y b n | a + n < b = f' x (a+x) y b n
f' x a y b n | a + n > b = f' x a y (b+y) n
f' _ _ _ _ _ = error "I wasn't there"

g x y n k = x*y*k + c
  where c = f x y n

values (x,y,n) = map (g x y n) [0..]

findFirstCommon :: [[Int64]] -> Int64
findFirstCommon ls = if (all (\xs -> (head xs) == m) ls)
                        then m
                        else trace (show m) (findFirstCommon (map (catchup m) ls))
  where m = maximum $ map head ls
        catchup n (x:xs) | x < n = catchup n xs
        catchup _ xs = xs

-- primes :: [Int]
-- primes = f [2..]
--   where f (p : ns) = p : f [n | n <- ns, n `mod` p /= 0]
--         f _ = error "you don't see me"

-- factors m = f m (head primes) (tail primes) where
--   f m n ns
--     | m < 2 = []
--     | m < n ^ 2 = [m]   -- stop early
--     | m `mod` n == 0 = n : f (m `div` n) n ns
--     | otherwise = f m (head ns) (tail ns)
