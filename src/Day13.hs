module Day13 where

import Data.Int
import Data.List
import Data.List.Split

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
partTwo = solve

f x y n = f' x x y y n

f' _ a _ b n | a + n == b = a
f' x a y b n | a + n < b = f' x (a+x) y b n
f' x a y b n | a + n > b = f' x a y (b+y) n
f' _ _ _ _ _ = error "I wasn't there"

toAffineCoeffs (x,y,n) = (x*y, b)
  where b = f x y n

isGoodAt k (_,p,d) = 0 == (mod (k+d) p)

findBestCoeffs xs = head $ sortBy (\(a,_) (b,_) -> compare b a) $ map toAffineCoeffs xs

walkNs a b xs n = if all (isGoodAt k) xs then k else (walkNs a b xs $ n+1)
  where k = a*n+b

solve xs = walkNs a b xs 1
  where (a, b) = findBestCoeffs xs

--------------------------------
--

--   100,000,000,000,000
-- 1,012,171,816,131,114
-- 1,936,728,611,590,279

-- partTwo = findFirstCommon . (map values)

-- g x y n k = x*y*k + c
--   where c = f x y n

-- values (x,y,n) = map (g x y n) [0..]
-- 
-- findFirstCommon :: [[Int64]] -> Int64
-- findFirstCommon ls = if (all (\xs -> (head xs) == m) ls)
--                         then m
--                         else trace (show m) (findFirstCommon (map (catchup m) ls))
--   where m = maximum $ map head ls
--         catchup n (x:xs) | x < n = catchup n xs
--         catchup _ xs = xs
-- 
-- primes :: [Int64]
-- primes = f [2..]
--   where f (p : ns) = p : f [n | n <- ns, n `mod` p /= 0]
--         f _ = error "you don't see me"
-- 
-- factors :: Int64 -> [Int64]
-- factors m = f m (head primes) (tail primes) where
--   f m n ns
--     | m < 2 = []
--     | m < n ^ 2 = [m]   -- stop early
--     | m `mod` n == 0 = n : f (m `div` n) n ns
--     | otherwise = f m (head ns) (tail ns)
-- 
-- input :: [(Int64,Int64,Int64)]
-- input = [(19,37,13)
--         ,(19,599,19)
--         ,(19,29,21)
--         ,(19,17,36)
--         ,(19,23,42)
--         ,(19,761,50)
--         ,(19,41,60)
--         ,(19,13,63)]
--
-- distanceToNext p k = p - (mod k p)
