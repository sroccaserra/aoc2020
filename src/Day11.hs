module Day11 where

import Data.Vector (Vector, length, fromList, toList, imap, (!))

main = interact $ show . partTwo . lines

partOne = countEmptyRoomSeats . stepUntilStale . asRoom

partTwo = countEmptyRoomSeats . stepUntilStale' . asRoom

type Room = Vector Row
type Row = Vector Char

asRoom :: [String] -> Room
asRoom = fromList . map fromList

width r = Data.Vector.length (r!0)
height r = Data.Vector.length r

countEmptyRoomSeats room = occupiedSeats s
  where s = concat $ map toList $ toList room

stepUntilStale room = if room == next then room else stepUntilStale next
  where next = step room

stepUntilStale' = stepUntilStale

step :: Room -> Room
step room = imap (\y r -> imap (\x _ -> stepSeat room x y) r) room

stepSeat room x y = case c of
  '.' -> '.'
  'L' | 0 == (occupiedSeats $ adjacentSeats room x y) -> '#'
  '#' | 4 <= (occupiedSeats $ adjacentSeats room x y) -> 'L'
  c -> c
  where (c:_) = seat room x y

occupiedSeats :: String -> Int
occupiedSeats xs = Prelude.length $ filter (== '#') xs

adjacentSeats :: Room -> Int -> Int -> String
adjacentSeats room x y = concat $ a++b++c
  where a = [seat room (x-1) (y-1),seat room x $ y-1,seat room (x+1) (y-1)]
        b = [seat room (x-1) (y),seat room (x+1) y]
        c = [seat room (x-1) (y+1),seat room x $ y+1,seat room (x+1) (y+1)]

seat :: Room -> Int -> Int -> String
seat room x y | x < 0 || y < 0 || x >= w || y >= h = ""
  where w = width room
        h = height room
seat room x y = [(room ! y) ! x]

seeSeat room x y slope = seeSeat' w h room (x + (fst slope)) (y + (snd slope)) slope
  where w = width room
        h = height room

seeSeat' :: Int -> Int -> Room -> Int -> Int -> (Int, Int) -> Char
seeSeat' w h room x y slope | x >= w || x < 0  || y >= h || y < 0 = '.'
seeSeat' w h room x y slope = if "." /= s then head s else  seeSeat' w h room (x + (fst slope)) (y + (snd slope)) slope
  where s = seat room x y
