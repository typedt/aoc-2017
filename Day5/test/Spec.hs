import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Lib

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do
  describe "Part 1" $ do
    it "example input 1" $ do
      let input = "0\n3\n0\n1\n-3\n"
      part1 input `shouldBe` 5

    it "real input" $ do
      realInput <- fmap init $ readFile "test/input.txt"
      part1 realInput `shouldBe` 325922

  describe "Part 2" $ do
    it "example input 1" $ do
      let input = "0\n3\n0\n1\n-3\n"
      part2 input `shouldBe` 10

    it "real input" $ do
      realInput <- fmap init $ readFile "test/input.txt"
      part2 realInput `shouldBe` 24490906
