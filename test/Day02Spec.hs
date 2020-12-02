module Day02Spec where

import Day02 hiding (main)

import Test.Hspec

spec = describe "Parsing a line" $ do
  it "reads a list of numbers" $ do
    let result = parseLine "1-20 a: aaa"
    result `shouldBe` PasswordInfo 1 20 'a' "aaa"
