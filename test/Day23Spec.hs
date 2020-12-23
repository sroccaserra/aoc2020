module Day23Spec where

import Day23 hiding (main)
import qualified Data.Sequence as Seq

import Test.Hspec

main = hspec spec

spec =
  describe "Crab game" $ do
    describe "Part one" $ do
      it "matches the example" $ do
        let result = take 10 $ iterate step $ Seq.fromList [3,8,9,1,2,5,4,6,7]
        result `shouldBe` map Seq.fromList [[3,8,9,1,2,5,4,6,7]
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
        (step $ Seq.fromList [3,8,9,1,7,5,4,6,2]) `shouldBe` Seq.fromList [7,5,4,6,2,8,9,1,3]

      it "skips removed cups" $ do
        (step $ Seq.fromList [9,8,7,6,1,5,4,3,2]) `shouldBe` Seq.fromList [1,5,8,7,6,4,3,2,9]

      it "wraps around to 9" $ do
        (step $ Seq.fromList [1,9,8,7,6,5,4,3,2]) `shouldBe` Seq.fromList [6,9,8,7,5,4,3,2,1]

      it "has the right 10th step" $ do
        let result = last $ take 11 $ iterate step $ Seq.fromList [3,8,9,1,2,5,4,6,7]
        alignToOne result `shouldBe` Seq.fromList [1,9,2,6,5,8,3,7,4]

    describe "Part two" $ do
      it "can multiply big numbers" $ do
        934001*159792 `shouldBe` 149245887792
