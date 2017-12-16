import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Lib

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do
  describe "Part 1" $ do
    it "sum of example input 1" $ do
      let input = "aa bb cc dd ee"
      part1 input `shouldBe` 1

    it "sum of example input 2" $ do
      let input = "aa bb cc dd aa"
      part1 input `shouldBe` 0

    it "sum of example input 3" $ do
      let input = "aa bb cc dd aaa"
      part1 input `shouldBe` 1

    it "sum of real input" $ do
      realInput <- fmap init $ readFile "test/input.txt"
      part1 realInput `shouldBe` 477

  describe "Part 2" $ do
    it "sum of example input 1" $ do
      let input = "aa bb cc dd ee"
      part2 input `shouldBe` 1

    it "sum of real input" $ do
      realInput <- fmap init $ readFile "test/input.txt"
      part2 realInput `shouldBe` 167
