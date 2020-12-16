module Day16 where

import Data.Char
import Data.List.Split

main = interact $ show . partOne . parse

parse xs = (map parseRule a, map parseTicket $ tail $ init c)
  where (a:_:c:_) = map (splitOn "\n") $ splitOn "\n\n" xs

parseRule s = (label , map read $ words $ map clean xs ::[Int])
  where parts = splitOn ":" s
        [label,xs] = parts
        clean c | isDigit c = c
        clean _ = ' '

parseTicket :: String -> [Int]
parseTicket = map read . splitOn ","

partOne (rules,tickets) = sum $ concat $ map (checkTicket rules) tickets

checkTicket rs xs = filter (not . matchesAnyRule rs) xs

matchesAnyRule rs x = any (matchesRule x) rs

matchesRule x (_,[a1,b1,a2,b2]) = (a1 <= x && x <= b1) || (a2 <= x && x <= b2)
matchesRule _ _ = error "wrong input"
