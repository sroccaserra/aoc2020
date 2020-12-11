module Day11Spec where

import Day11 hiding (main)

import Test.Hspec

import Data.Vector (Vector, fromList, toList, (!))

main = hspec spec

room1 = asRoom ["LLL","LLL","LLL"]

exampleRoom = asRoom [
  "L.LL.LL.LL",
  "LLLLLLL.LL",
  "L.L.L..L..",
  "LLLL.LL.LL",
  "L.LL.LL.LL",
  "L.LLLLL.LL",
  "..L.L.....",
  "LLLLLLLLLL",
  "L.LLLLLL.L",
  "L.LLLLL.LL" ]

step1 = asRoom [
  "#.##.##.##",
  "#######.##",
  "#.#.#..#..",
  "####.##.##",
  "#.##.##.##",
  "#.#####.##",
  "..#.#.....",
  "##########",
  "#.######.#",
  "#.#####.##"]

step2 = asRoom [
  "#.LL.L#.##",
  "#LLLLLL.L#",
  "L.L.L..L..",
  "#LLL.LL.L#",
  "#.LL.LL.LL",
  "#.LLLL#.##",
  "..L.L.....",
  "#LLLLLLLL#",
  "#.LLLLLL.L",
  "#.#LLLL.##"]

step3 = asRoom [
  "#.##.L#.##",
  "#L###LL.L#",
  "L.#.#..#..",
  "#L##.##.L#",
  "#.##.LL.LL",
  "#.###L#.##",
  "..#.#.....",
  "#L######L#",
  "#.LL###L.L",
  "#.#L###.##"]

step5 = asRoom [
  "#.#L.L#.##",
  "#LLL#LL.L#",
  "L.#.L..#..",
  "#L##.##.L#",
  "#.#L.LL.LL",
  "#.#L#L#.##",
  "..L.L.....",
  "#L#L##L#L#",
  "#.LLLLLL.L",
  "#.#L#L#.##"]

lastStep = asRoom [
  "#.#L.L#.##",
  "#LLL#LL.L#",
  "L.#.L..#..",
  "#L##.##.L#",
  "#.#L.LL.LL",
  "#.#L#L#.##",
  "..L.L.....",
  "#L#L##L#L#",
  "#.LLLLLL.L",
  "#.#L#L#.##"]

spec =
  describe "Day 11" $ do
    it "finds a seat" $ do
      seat (asRoom ["123","456","789"]) 1 1 `shouldBe` "5"

    it "finds adjacent seat" $ do
      adjacentSeats (asRoom["123","456","789"]) 1 1 `shouldBe` "12346789"
      adjacentSeats (asRoom["123","456","789"]) 0 0 `shouldBe` "245"
      adjacentSeats (asRoom["123","456","789"]) 2 2 `shouldBe` "568"

    it "steps a seat" $ do
      stepSeat room1 0 0 `shouldBe` '#'

    it "generates next step" $ do
      step room1 `shouldBe` asRoom ["###","###","###"]
      (step step1) ! 0  `shouldBe` fromList "#.LL.L#.##"

    it "check step 2 computation" $ do
      adjacentSeats step1 0 0 `shouldBe` ".##"
      occupiedSeats (adjacentSeats step1 0 0) `shouldBe` 2
      4 <= (occupiedSeats (adjacentSeats step1 0 0)) `shouldBe` False

    it "generates steps for example room" $ do
      step exampleRoom `shouldBe` step1
      step step1 `shouldBe` step2
      step step2 `shouldBe` step3
      step step5 `shouldBe` lastStep
      step lastStep `shouldBe` lastStep

    it "steps until stale" $ do
      stepUntilStale exampleRoom `shouldBe` lastStep

    it "counts occupied seats in a room" $ do
      countEmptyRoomSeats lastStep `shouldBe` 37

    it "builds a vector" $ do
      let r = fromList [fromList "LLL", fromList "LLL"] :: Vector (Vector Char)
      let rr = room1
      r ! 0 ! 0 `shouldBe` 'L'
      rr ! 0 ! 0 `shouldBe` 'L'
