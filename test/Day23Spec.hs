module Day23Spec where

import Day23 hiding (main)

import Test.Hspec

main = hspec spec

spec =
  describe "Crab game" $ do
    it "matches the example" $ do
      let result = take 10 $ iterate step [3,8,9,1,2,5,4,6,7]
      result `shouldBe` [[3,8,9,1,2,5,4,6,7]
                        ,[2,8,9,1,5,4,6,7,3]
                        ,[5,4,6,7,8,9,1,3,2]
                        ,[8,9,1,3,4,6,7,2,5]
                        ,[4,6,7,9,1,3,2,5,8]
                        ,[1,3,6,7,9,2,5,8,4]
                        ,[9,3,6,7,2,5,8,4,1]
                        ,[2,5,8,3,6,7,4,1,9]
                        ,[6,7,4,1,5,8,3,9,2]
                        ,[5,7,4,1,8,3,9,2,6]]

    it "can place removed cups last" $ do
      step [3,8,9,1,7,5,4,6,2] `shouldBe` [7,5,4,6,2,8,9,1,3]

    it "skips removed cups" $ do
      step [9,8,7,6,1,5,4,3,2] `shouldBe` [1,5,8,7,6,4,3,2,9]

    it "wraps around to 9" $ do
      step [1,9,8,7,6,5,4,3,2] `shouldBe` [6,9,8,7,5,4,3,2,1]

    it "has the right 10th step" $ do
      let result = last $ take 11 $ iterate step [3,8,9,1,2,5,4,6,7]
      alignToOne result `shouldBe` [1,9,2,6,5,8,3,7,4]
