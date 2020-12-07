{-# LANGUAGE OverloadedStrings #-}

module Day07 where

import qualified Data.Text as T

-- main = interact $ show . partOne . (map parseLine) . lines
main = do
  xs <- fmap lines getContents
  mapM_ print $ map parseLine xs


parseLine :: String -> (T.Text, [(Int, T.Text)])
parseLine = parseQuantities . T.splitOn ":" . T.pack

parseQuantities :: [T.Text] -> (T.Text, [(Int, T.Text)])
parseQuantities xs = ((xs !! 0), qs)
  where qs = (filter $ (/= 0) . fst) $ map parseQuantity $ T.splitOn "," $ xs !! 1

parseQuantity :: T.Text -> (Int, T.Text)
parseQuantity "" = (0,"")
parseQuantity x = (read (T.unpack $ ws !! 0), ws !! 1)
  where ws = T.words x

partOne = head
