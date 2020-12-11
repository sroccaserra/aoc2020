module Day11 where

import Data.Maybe
import Data.Vector (Vector, length, fromList, toList, imap, (!))

main = interact $ show . partTwo . lines

partOne = countEmptyRoomSeats . stepUntilStable stepSeat . asRoom

partTwo = countEmptyRoomSeats . stepUntilStable stepSeat' . asRoom

type Room = Vector Row
type Row = Vector Char

asRoom :: [String] -> Room
asRoom = fromList . map fromList

width r = Data.Vector.length (r!0)
height r = Data.Vector.length r

directions = [(x,y) | x <- [-1..1], y <- [-1..1], (x,y) /= (0,0)]

countEmptyRoomSeats room = occupiedSeats s
  where s = concat $ map toList $ toList room

stepUntilStable f room = if room == next then room else stepUntilStable f next
  where next = step f room

step f room = imap (\y r -> imap (\x _ -> f room x y) r) room

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

occupiedSeats :: [Char] -> Int
occupiedSeats xs = Prelude.length $ filter (== '#') xs

adjacentSeats :: Room -> Int -> Int -> [Char]
adjacentSeats room x y = catMaybes $ map (uncurry $ seat room) xys
  where xys = [(x+i,y+j) | (i,j) <- directions]

visibleSeats :: Room -> Int -> Int -> [Char]
visibleSeats room x y = catMaybes $ map (seeSeat room x y) directions

seat :: Room -> Int -> Int -> Maybe Char
seat room x y | isOutOfBound w h x y = Nothing
  where w = width room
        h = height room
seat room x y = Just $ (room ! y) ! x

seeSeat room x y slope = seeSeat' w h room (x + (fst slope)) (y + (snd slope)) slope
  where w = width room
        h = height room

seeSeat' :: Int -> Int -> Room -> Int -> Int -> (Int, Int) -> Maybe Char
seeSeat' w h _ x y _ | isOutOfBound w h x y = Nothing
seeSeat' w h room x y slope = if Just '.' /= c then c else  seeSeat' w h room (x + (fst slope)) (y + (snd slope)) slope
  where c = seat room x y

isOutOfBound w h x y = x < 0 || x >= w || y < 0 || y >= h
