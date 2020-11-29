module DayXXSpec where

import DayXX (processLine)

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Processing a line" $ do
    it "reads a list of numbers" $ do
      let result = processLine "1 2 3"
      result `shouldBe` [1, 2, 3]
