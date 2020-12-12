module Day12Two where

main = interact $ show . partTwo . (map parseLine) . lines

parseLine (d:x) = (d,read x ::Int)
parseLine s = error $ "wrong input: " ++ s

partTwo = manhattanDistance . foldl moveShip (10,1,0,0)
  where manhattanDistance (_,_, x, y) = abs x + abs y

moveShip (u,v,x,y) ('N',n) = (u,v+n,x,y)
moveShip (u,v,x,y) ('S',n) = (u,v-n,x,y)
moveShip (u,v,x,y) ('E',n) = (u+n,v,x,y)
moveShip (u,v,x,y) ('W',n) = (u-n,v,x,y)
moveShip s ('F',n) = advanceShip n s
moveShip s ('R',a) = turnShipRight a s
moveShip s ('L',a) = turnShipLeft a s
moveShip _ _ = error "wrong instruction"

advanceShip n (u,v,x,y) = (u,v,x+n*u, y+n*v)

turnShipRight a (u,v,x,y) | a > 90 = turnShipRight (a-90) (v,-u,x,y)
turnShipRight _ (u,v,x,y) = (v,-u,x,y)

turnShipLeft a (u,v,x,y) | a > 90 = turnShipLeft (a-90) (-v,u,x,y)
turnShipLeft _ (u,v,x,y) = (-v,u,x,y)
