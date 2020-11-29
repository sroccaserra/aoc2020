module Day01Spec where

import Test.Hspec

main = hspec $ do
  describe "A failing test" $ do
    it "fails" $ do
      5 `shouldBe` 6
