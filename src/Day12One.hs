module Day12One where

import Data.Maybe

main = interact $ show . partOne . (map parseLine) . lines

parseLine (d:x) = (d,read x ::Int)
parseLine s = error $ "wrong input: " ++ s

partOne = manhattanDistance . applyInstructions ('E',0,0)
-- partOne = applyInstructions ('E',0,0)

applyInstructions s (x:[]) = moveShip x s
applyInstructions s (x:rest) = applyInstructions (moveShip x s) rest
applyInstructions _ _ = error "wrong instruction"

manhattanDistance (_, x, y) = abs x + abs y

type Ship = (Char,Int,Int)

moveShip ('N',n) (d,x,y) = (d, x, y+n)
moveShip ('S',n) (d,x,y) = (d, x, y-n)
moveShip ('E',n) (d,x,y) = (d, x+n, y)
moveShip ('W',n) (d,x,y) = (d, x-n, y)
moveShip ('F',n) s = advanceShip n s
moveShip ('R',a) s = turnShipRight a s
moveShip ('L',a) s = turnShipLeft a s
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
