module Day12One where

import Data.Maybe

main = interact $ show . partOne . (map parseLine) . lines

parseLine (d:x) = (d,read x ::Int)
parseLine s = error $ "wrong input: " ++ s

partOne = manhattanDistance . foldl moveShip ('E', 0,0)
  where manhattanDistance (_, x, y) = abs x + abs y

moveShip (d,x,y) ('N',n) = (d, x, y+n)
moveShip (d,x,y) ('S',n) = (d, x, y-n)
moveShip (d,x,y) ('E',n) = (d, x+n, y)
moveShip (d,x,y) ('W',n) = (d, x-n, y)
moveShip s ('F',n) = advanceShip n s
moveShip s ('R',a) = turnShipRight a s
moveShip s ('L',a) = turnShipLeft a s
moveShip _ _ = error "wrong instruction"

advanceShip n (d@'N',x,y) = (d, x, y+n)
advanceShip n (d@'S',x,y) = (d, x, y-n)
advanceShip n (d@'E',x,y) = (d, x+n, y)
advanceShip n (d@'W',x,y) = (d, x-n, y)
advanceShip _ _ = error "wrong direction"

turnShipRight a (d,x,y) | a > 90 = turnShipRight (a-90) (d',x,y)
  where d' = fromJust $ lookup d directionsRTL
turnShipRight _ (d,x,y) = (d',x,y)
  where d' = fromJust $ lookup d directionsRTL

turnShipLeft a (d,x,y) | a > 90 = turnShipLeft (a-90) (d',x,y)
  where d' = fromJust $ lookup d directionsLTR
turnShipLeft _ (d,x,y) = (d',x,y)
  where d' = fromJust $ lookup d directionsLTR

directionsRTL = [('E','S'), ('S', 'W'), ('W', 'N'), ('N', 'E')]
directionsLTR = [('S','E'), ( 'W','S'), ( 'N','W'), ( 'E','N')]
