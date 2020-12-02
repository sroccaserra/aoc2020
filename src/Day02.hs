module Day02 where

import Data.Function

main :: IO ()
main = do
  inputLines <- fmap lines getContents
  print $ inputLines & map parseLine & partTwo

parseLine :: String -> PasswordInfo
parseLine x = PasswordInfo (read x1 ::Int) (read x2 ::Int) (head x3) x4
  where [x1, x2, x3, x4] = x & map clean & words
        clean '-' = ' '
        clean ':' = ' '
        clean c = c

partOne :: [PasswordInfo] -> Int
partOne = length . filter isValidPartOne

partTwo :: [PasswordInfo] -> Int
partTwo = length . filter isValidPartTwo

data PasswordInfo = PasswordInfo Int Int Char String
                  deriving (Show, Eq)

isValidPartOne :: PasswordInfo -> Bool
isValidPartOne (PasswordInfo n1 n2 c p) = n1 <= n && n <= n2
  where n = p & filter (== c) & length

isValidPartTwo :: PasswordInfo -> Bool
isValidPartTwo (PasswordInfo n1 n2 c p) =
  (c1 == c && c2 /= c) || (c1 /= c && c2 == c)
  where
    c1 = p !! (n1-1)
    c2 = p !! (n2-1)
