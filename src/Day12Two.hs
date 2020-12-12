module Day12Two where

import Data.Complex

main = interact $ show . partTwo . (map parseLine) . lines

parseLine (d:x) = (d,read x ::Float)
parseLine s = error $ "wrong input: " ++ s

partTwo = manhattanDistance . foldl moveShip (10:+1, 0:+0)
  where manhattanDistance (_, x:+y) = abs x + abs y

j = 0:+1

moveShip (w,p) ('N',x) = (w + (0:+x), p)
moveShip (w,p) ('S',x) = (w - (0:+x), p)
moveShip (w,p) ('E',x) = (w + (x:+0), p)
moveShip (w,p) ('W',x) = (w - (x:+0), p)
moveShip (w,p) ('F',x) = (w, (x:+0)*w+p)
moveShip (w,p) ('R',x) = ((-j)^(round x `div` 90)*w, p)
moveShip (w,p) ('L',x) = (j^(round x `div` 90)*w, p)
moveShip _ _ = error "wrong instruction"
