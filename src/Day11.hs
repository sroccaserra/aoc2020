module Day11 where

main = interact $ show . partOne . lines

partOne = occupiedSeats . concat . stepUntilStale

stepUntilStale room = if room == next then room else stepUntilStale next
  where next = step room

step :: [String] -> [String]
step room@(a:_) = [[stepSeat room x y | x <- [0..w-1]] | y <- [0..h-1]]
  where w = length a
        h = length room
step _ = error "empty room irrelevant"

stepSeat room x y = case c of
  '.' -> '.'
  'L' | 0 == (occupiedSeats $ adjacentSeats room x y) -> '#'
  '#' | 4 <= (occupiedSeats $ adjacentSeats room x y) -> 'L'
  c -> c
  where (c:_) = seat room x y

occupiedSeats :: String -> Int
occupiedSeats xs = length $ filter (== '#') xs

adjacentSeats :: [String] -> Int -> Int -> String
adjacentSeats room x y = concat $ a++b++c
  where a = [seat room (x-1) (y-1),seat room x $ y-1,seat room (x+1) (y-1)]
        b = [seat room (x-1) (y),seat room (x+1) y]
        c = [seat room (x-1) (y+1),seat room x $ y+1,seat room (x+1) (y+1)]

seat :: [String] -> Int -> Int -> String
seat room@(a:_) x y | x < 0 || y < 0 || x >= w || y >= h = ""
  where w = length a
        h = length room
seat room x y = [(room !! y) !! x]
