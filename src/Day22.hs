module Day22 where

-- main = interact $ unlines . (map show) . partOne . (map parseLine) . lines

main = print partOne


player1Deck = [28,3,35,27,19,40,14,15,17,22,45,47,26,13,32,38,43,24,29,5,31,48,49,41,25]
player2Deck = [34,12,2,50,16,1,44,11,36,6,10,42,20,8,46,9,37,4,7,18,23,39,30,33,21]

partOne = sum $ zipWith (*) (reverse [1..50]) finalRound
  where finalRound = combat player1Deck player2Deck

combat [] p2d = p2d
combat p1d [] = p1d
combat (x:p1d) (y:p2d) | x > y = combat (p1d++[x,y]) p2d
combat (x:p1d) (y:p2d) = combat p1d (p2d++[y,x])
