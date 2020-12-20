module Day20Spec where

import Day20 hiding (main)

import Test.Hspec

main = hspec spec

tile_1117_s =
  " Tile 1117: \n\
  \ #.#.##...# \n\
  \ .........# \n\
  \ .........# \n\
  \ #....#..## \n\
  \ #.#.##..#. \n\
  \ #......#.. \n\
  \ #.#.#.#... \n\
  \ #..#.....# \n\
  \ ......#.#. \n\
  \ ..##.#..#. \n"

tile_2003_s =
  " Tile 2003: \n\
  \ ..#.#....# \n\
  \ .##..#...# \n\
  \ #......... \n\
  \ #......... \n\
  \ #......... \n\
  \ #.##...#.. \n\
  \ #..#..##.# \n\
  \ .......#.. \n\
  \ ##.......# \n\
  \ ..#####..# \n"

tile_1559_s =
  " Tile 1559: \n\
  \ ##.##.#..# \n\
  \ .....#.#.. \n\
  \ #.#.####.. \n\
  \ ...#.#...# \n\
  \ ##....#..# \n\
  \ #.##...#.. \n\
  \ .#..##.### \n\
  \ ....#..#.. \n\
  \ .#.#.#.#.# \n\
  \ #....#..#. \n"

tile_small_s =
  " Tile 1: \n\
  \ #.. \n\
  \ ##. \n\
  \ .#. \n"

spec =
  describe "Day 20" $ do
    it "parses an example" $ do
      let ((Tile i xs):_) = parseTiles tile_1117_s
      i `shouldBe` 1117
      head xs `shouldBe` "#.#.##...#"

    it "parses two examples" $ do
      let ts = parseTiles (tile_1117_s ++ tile_2003_s)
      length ts `shouldBe` 2

    it "aligns to another tile" $ do
      let tile_1117:tile_2003:_ = parseTiles $ tile_1117_s ++ tile_2003_s
      let Just (Tile _ xs) = alignTo tile_1117 tile_2003
      head xs `shouldBe` "##....#.##"

    it "aligns to another tile" $ do
      let tile_1117:tile_1559:_ = parseTiles $ tile_1117_s ++ tile_1559_s
      let Just (Tile _ xs) = alignTo tile_1117 tile_1559
      head xs `shouldBe` "#..##.#.#."

    it "flips a tile" $ do
      let [t] = parseTiles tile_small_s
      let (Tile _ xs) = flipTile t
      xs `shouldBe` ["..#",".##",".#."]
