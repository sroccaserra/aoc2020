module Day02 where

import Data.Function

import Text.Regex.TDFA

main :: IO ()
main = do
  inputLines <- fmap lines getContents
  print $ inputLines & map parseLine & partOne

parseLine :: String -> PasswordInfo
parseLine x = PasswordInfo (read x1) (read x2) (head x3) x4
  where (_, _, _, [x1, x2, x3, x4]) =
          x =~ "([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)" :: (String, String, String, [String])

partOne :: [PasswordInfo] -> Int
partOne = length . filter isValid

data PasswordInfo = PasswordInfo Int Int Char String
                  deriving (Show, Eq)

isValid :: PasswordInfo -> Bool
isValid (PasswordInfo n1 n2 c p) = n1 <= n && n <= n2
  where n = p & filter (== c) & length
