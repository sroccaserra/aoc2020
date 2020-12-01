module DayXXSpec where

import DayXX hiding (main)

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Parsing a line" $ do
    it "reads a list of numbers" $ do
      let result = parse ["1", "2", "3"]
      result `shouldBe` [1, 2, 3]
