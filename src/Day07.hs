{-# LANGUAGE OverloadedStrings #-}

module Day07 where

import Data.Maybe
import qualified Data.Text as T

main = interact $ show . partTwo . (map parseLine) . lines

parseLine :: String -> Rule
parseLine = parseQuantities . T.splitOn ":" . T.pack

parseQuantities :: [T.Text] -> Rule
parseQuantities xs = ((xs !! 0), qs)
  where qs = (filter $ (/= 0) . snd) $ map parseQuantity $ T.splitOn "," $ xs !! 1

parseQuantity :: T.Text -> Quantity
parseQuantity "" = ("", 0)
parseQuantity x = (ws !! 1, read (T.unpack $ ws !! 0))
  where ws = T.words x

partOne m = length $ filter id $ map (\x -> canContainShinyGold m (fst x)) m

partTwo m = (countBags m $ findQuantities m "shiny_gold") - 1

type Quantity = (T.Text, Int)
type Rule = (T.Text, [Quantity])

containsShinyGoldDirect :: [Quantity] -> Bool
containsShinyGoldDirect = isJust . lookup "shiny_gold"

canContainShinyGold :: [Rule] -> T.Text -> Bool
canContainShinyGold m s | containsShinyGoldDirect $ findQuantities m s = True
canContainShinyGold m s | [] == findQuantities m s = False
canContainShinyGold m s = any (canContainShinyGold m) xs
  where xs = map fst $ findQuantities m s

countBags :: [Rule] -> [Quantity] -> Int
countBags _ [] = 1
countBags m qs = succ $ foldl acc 0 qs
  where acc a (s, n) = a + n * (countBags m (findQuantities m s))

findQuantities m s = fromMaybe [] $ lookup s m
