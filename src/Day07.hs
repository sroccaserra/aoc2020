{-# LANGUAGE OverloadedStrings #-}

module Day07 where

import Data.Maybe
import qualified Data.Text as T

main = interact $ show . partTwo . (map parseLine) . lines

parseLine :: String -> (T.Text, [Quantity])
parseLine = parseQuantities . T.splitOn ":" . T.pack

parseQuantities :: [T.Text] -> (T.Text, [Quantity])
parseQuantities xs = ((xs !! 0), qs)
  where qs = (filter $ (/= 0) . snd) $ map parseQuantity $ T.splitOn "," $ xs !! 1

parseQuantity :: T.Text -> Quantity
parseQuantity "" = ("", 0)
parseQuantity x = (ws !! 1, read (T.unpack $ ws !! 0))
  where ws = T.words x

partOne m = length $ filter id $ map (\x -> canContainShinyGold m (fst x)) m

partTwo m = (countBags m $ findQuantities m "shiny_gold") - 1

type Quantity = (T.Text, Int)

containsShinyGoldDirect (Just xs) = isJust $ lookup "shiny_gold" xs
containsShinyGoldDirect Nothing = False

canContainShinyGold :: [(T.Text, [Quantity])] -> T.Text -> Bool
canContainShinyGold m c | containsShinyGoldDirect $ lookup c m = True
canContainShinyGold m c | Nothing == lookup c m = False
canContainShinyGold m c | Just [] == lookup c m = False
canContainShinyGold m c = any (canContainShinyGold m) xs
  where xs = map fst $ fromJust $ lookup c m

countBags :: [(T.Text, [Quantity])] ->[Quantity] -> Int
countBags _ [] = 1
countBags m qs = 1 + sum xs
  where xs::[Int]
        xs = map (\(s,n) -> n * (countBags m (findQuantities m s))) qs

findQuantities m s = fromMaybe [] $ lookup s m
