import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Lib

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do
  describe "Part 1" $ do
    it "example input 1" $ do
      let input = "0\t2\t7\t0"
      part1 input `shouldBe` 5

    it "real input" $ do
      realInput <- fmap init $ readFile "test/input.txt"
      part1 realInput `shouldBe` 11137

  describe "Part 2" $ do
    it "example input 1" $ do
      let input = "0\t2\t7\t0"
      part2 input `shouldBe` 4

    it "real input" $ do
      realInput <- fmap init $ readFile "test/input.txt"
      part2 realInput `shouldBe` 1037
