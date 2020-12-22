module Day22 where

import qualified Data.Set as S
import qualified Data.Map.Strict as M

main = print partTwo

player1Deck = [28,3,35,27,19,40,14,15,17,22,45,47,26,13,32,38,43,24,29,5,31,48,49,41,25]
player2Deck = [34,12,2,50,16,1,44,11,36,6,10,42,20,8,46,9,37,4,7,18,23,39,30,33,21]

player1DeckEx = [9,2,6,3,1]
player2DeckEx = [5,8,4,7,10]

partOne = score finalRound
  where finalRound = combat player1Deck player2Deck

partTwo = score finalRound
  where finalRound = combatRec M.empty S.empty player1Deck player2Deck
  -- where finalRound = combatRec M.empty S.empty player1DeckEx player2DeckEx

score deck = sum $ zipWith (*) (reverse [1..length deck]) deck

combat [] p2d = p2d
combat p1d [] = p1d
combat (x:p1d) (y:p2d) | x > y = combat (p1d++[x,y]) p2d
combat (x:p1d) (y:p2d) = combat p1d (p2d++[y,x])

combatRec _ _ [] p2d = p2d
combatRec _ _ p1d [] = p1d
combatRec _ s p1d p2d | S.member (p1d,p2d) s = p1d
combatRec m s (x:p1d) (y:p2d) | isSubgame (x:p1d) (y:p2d) =
  combatRec m' s' (p1d++xs) (p2d++ys)
  where (xs,ys,m') = subgame m S.empty (x,y,(p1d,p2d)) p1d p2d
        s' = S.insert ((x:p1d), (y:p2d)) s
combatRec m s (x:p1d) (y:p2d) | x > y = combatRec m s' (p1d++[x,y]) p2d
  where s' = S.insert ((x:p1d), (y:p2d)) s
combatRec m s (x:p1d) (y:p2d) = combatRec m s' p1d (p2d++[y,x])
  where s' = S.insert ((x:p1d), (y:p2d)) s

isSubgame (x:p1d) (y:p2d) = x <= (length p1d) && y <= (length p2d)
isSubgame _ _ = error "wrong subgame"

subgame :: M.Map ([Int],[Int]) Player -> S.Set ([Int],[Int]) -> (Int,Int,([Int],[Int])) -> [Int] -> [Int] -> ([Int],[Int],M.Map ([Int],[Int]) Player)
subgame m _ (x,y,decks) _ _ | M.member decks m = if w == P1 then ([x,y],[],m) else ([],[y,x],m)
  where w = m M.! decks
subgame m _ (x,y,decks) [] _ = ([],[y,x],m')
  where m' = M.insert decks P2 m
subgame m _ (x,y,decks) _ [] = ([x,y],[],m')
  where m' = M.insert decks P1 m
subgame m s (x,y,decks) p1d p2d | S.member (p1d,p2d) s = ([x,y],[],m')
  where m' = M.insert decks P1 m

subgame m s (x,y,decks) (a:p1d) (b:p2d) | isSubgame (a:p1d) (b:p2d) =
  subgame m' s' (x,y,decks) (p1d++xs) (p2d++ys)
  where (xs,ys,m') = subgame m S.empty (a,b,(p1d,p2d)) p1d p2d
        s' = S.insert ((a:p1d),(b:p2d)) s

subgame m s (x,y,decks) (a:p1d) (b:p2d) | a > b = subgame m s' (x,y,decks) (p1d++[a,b]) p2d
  where s' = S.insert ((a:p1d),(b:p2d)) s

subgame m s (x,y,decks) (a:p1d) (b:p2d) = subgame m s' (x,y,decks) p1d (p2d++[b,a])
  where s' = S.insert ((a:p1d),(b:p2d)) s

data Player = P1 | P2
            deriving (Eq)
