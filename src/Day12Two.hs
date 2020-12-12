module Day12Two where

main = interact $ show . partTwo . (map parseLine) . lines

parseLine (d:x) = (d,read x ::Int)
parseLine s = error $ "wrong input: " ++ s

partTwo = manhattanDistance . applyInstructions (10,1,0,0)

applyInstructions s (x:[]) = moveShip x s
applyInstructions s (x:rest) = applyInstructions (moveShip x s) rest
applyInstructions _ _ = error "wrong instruction"

manhattanDistance (_,_, x, y) = abs x + abs y

type Ship = (Int,Int,Int,Int)

moveShip ('N',n) (u,v,x,y) = (u,v+n,x,y)
moveShip ('S',n) (u,v,x,y) = (u,v-n,x,y)
moveShip ('E',n) (u,v,x,y) = (u+n,v,x,y)
moveShip ('W',n) (u,v,x,y) = (u-n,v,x,y)
moveShip ('F',n) s = advanceShip n s
moveShip ('R',a) s = turnShipRight a s
moveShip ('L',a) s = turnShipLeft a s
moveShip _ _ = error "wrong instruction"

advanceShip n (u,v,x,y) = (u,v,x+n*u, y+n*v)

turnShipRight a (u,v,x,y) | a > 90 = turnShipRight (a-90) (v,-u,x,y)
turnShipRight _ (u,v,x,y) = (v,-u,x,y)

turnShipLeft a (u,v,x,y) | a > 90 = turnShipLeft (a-90) (-v,u,x,y)
turnShipLeft _ (u,v,x,y) = (-v,u,x,y)
