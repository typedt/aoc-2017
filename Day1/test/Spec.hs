import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Lib

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do
  describe "Part 1" $ do
    it "sum of example input 1" $ do
      let input = "1111"
      part1 input `shouldBe` 4

    it "sum of example input 2" $ do
      let input = "1234"
      part1 input `shouldBe` 0

    it "sum of example input 3" $ do
      let input = "9121212999912329"
      part1 input `shouldBe` 36

    it "sum of real input" $ do
      realInput <- fmap init $ readFile "test/input.txt"
      part1 realInput `shouldBe` 1177

  describe "Part 2" $ do
    it "sum of example input 1" $ do
      let input = "1212"
      part2 input `shouldBe` 6

    it "sum of real input" $ do
      realInput <- fmap init $ readFile "test/input.txt"
      part2 realInput `shouldBe` 1060
