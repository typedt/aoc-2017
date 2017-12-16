import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Lib

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

exampleFile = "test/example.txt"
inputFile = "test/input.txt"

specs :: Spec
specs = do
  describe "Part 1" $ do
    it "example input" $ do
      input <- fmap init $ readFile exampleFile
      part1 input `shouldBe` 1

    it "real input" $ do
      realInput <- fmap init $ readFile inputFile
      part1 realInput `shouldBe` 4902

  describe "Part 2" $ do
    it "example input" $ do
      input <- fmap init $ readFile exampleFile
      part2 input `shouldBe` 10

    it "real input" $ do
      realInput <- fmap init $ readFile inputFile
      part2 realInput `shouldBe` 7037
