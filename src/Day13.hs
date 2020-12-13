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
