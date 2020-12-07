{-# LANGUAGE OverloadedStrings #-}

module Day07 where

import Data.Maybe
import qualified Data.Text as T

main = interact $ show . partOne . (map parseLine) . lines
maino = do
  xs <- fmap lines getContents
  mapM_ print $ map parseLine xs


parseLine :: String -> (T.Text, [Quantity])
parseLine = parseQuantities . T.splitOn ":" . T.pack

parseQuantities :: [T.Text] -> (T.Text, [Quantity])
parseQuantities xs = ((xs !! 0), qs)
  where qs = (filter $ (/= 0) . snd) $ map parseQuantity $ T.splitOn "," $ xs !! 1

parseQuantity :: T.Text -> Quantity
parseQuantity "" = ("", 0)
parseQuantity x = (ws !!1, read (T.unpack $ ws !! 0))
  where ws = T.words x

partOne m = length $ filter id $ map (\x -> canContainShinyGold m (fst x)) m

type Quantity = (T.Text, Int)

containsShinyGoldDirect (Just xs) = isJust $ lookup "shiny_gold" xs
containsShinyGoldDirect Nothing = False

canContainShinyGold :: [(T.Text, [Quantity])] -> T.Text -> Bool
canContainShinyGold m c | containsShinyGoldDirect $ lookup c m = True
canContainShinyGold m c | Nothing == lookup c m = False
canContainShinyGold m c | Just [] == lookup c m = False
-- canContainShinyGold _ _ = False
canContainShinyGold m c = any (canContainShinyGold m) xs
  where xs = map fst $ fromJust $ lookup c m
