module DayXXSpec where

import DayXX hiding (main)

import Test.Hspec

spec = describe "Parsing a line" $ do
  it "reads a list of numbers" $ do
    let result = parseLine "1: 2 3"
    result `shouldBe` [1, 2, 3]
