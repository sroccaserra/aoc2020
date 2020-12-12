module Day12One where

import Data.Complex

main = interact $ show . partOne . (map parseLine) . lines

parseLine (d:x) = (d,read x ::Float)
parseLine s = error $ "wrong input: " ++ s

partOne = manhattanDistance . foldl moveShip (1:+0, 0:+0)
  where manhattanDistance (_, x:+y) = abs x + abs y

j = 0:+1

moveShip (d,p) ('N',x) = (d, (0:+x) + p)
moveShip (d,p) ('S',x) = (d, -(0:+x) + p)
moveShip (d,p) ('E',x) = (d, (x:+0) + p)
moveShip (d,p) ('W',x) = (d, -(x:+0) + p)
moveShip (d,p) ('F',x) = (d, (x:+0) * d + p)
moveShip (d,p) ('R',x) = ((-j)^(round x `div` 90)*d, p)
moveShip (d,p) ('L',x) = (j^(round x `div` 90)*d, p)
moveShip _ _ = error "wrong instruction"
