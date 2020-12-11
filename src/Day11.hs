module Day11 where

import Data.Maybe
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

directions = [(x,y) | x <- [-1..1], y <- [-1..1], (x,y) /= (0,0)]

countEmptyRoomSeats room = occupiedSeats s
  where s = concat $ map toList $ toList room

stepUntilStale room = if room == next then room else stepUntilStale next
  where next = step room

stepUntilStale' room = if room == next then room else stepUntilStale' next
  where next = step' room

step room = imap (\y r -> imap (\x _ -> stepSeat room x y) r) room
step' room = imap (\y r -> imap (\x _ -> stepSeat' room x y) r) room

stepSeat room x y =
  case c of
       '.' -> '.'
       'L' | 0 == (occupiedSeats $ adjacentSeats room x y) -> '#'
       '#' | 4 <= (occupiedSeats $ adjacentSeats room x y) -> 'L'
       c -> c
  where c = fromJust $ seat room x y

stepSeat' room x y =
  case c of
       '.' -> '.'
       'L' | 0 == (occupiedSeats $ visibleSeats room x y) -> '#'
       '#' | 5 <= (occupiedSeats $ visibleSeats room x y) -> 'L'
       c -> c
  where c = fromJust $ seat room x y

occupiedSeats :: String -> Int
occupiedSeats xs = Prelude.length $ filter (== '#') xs

adjacentSeats :: Room -> Int -> Int -> String
adjacentSeats room x y = catMaybes $ a++b++c
  where a = [seat room (x-1) (y-1),seat room x $ y-1,seat room (x+1) (y-1)]
        b = [seat room (x-1) (y),seat room (x+1) y]
        c = [seat room (x-1) (y+1),seat room x $ y+1,seat room (x+1) (y+1)]

visibleSeats :: Room -> Int -> Int -> String
visibleSeats room x y = catMaybes $ map (seeSeat room x y) directions

seat :: Room -> Int -> Int -> Maybe Char
seat room x y | x < 0 || y < 0 || x >= w || y >= h = Nothing
  where w = width room
        h = height room
seat room x y = Just $ (room ! y) ! x

seeSeat room x y slope = seeSeat' w h room (x + (fst slope)) (y + (snd slope)) slope
  where w = width room
        h = height room

seeSeat' :: Int -> Int -> Room -> Int -> Int -> (Int, Int) -> Maybe Char
seeSeat' w h _ x y _ | x >= w || x < 0  || y >= h || y < 0 = Nothing
seeSeat' w h room x y slope = if Just '.' /= c then c else  seeSeat' w h room (x + (fst slope)) (y + (snd slope)) slope
  where c = seat room x y
