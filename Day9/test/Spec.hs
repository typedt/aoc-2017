import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Lib

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

inputFile = "test/input.txt"

specs :: Spec
specs = do
  describe "Part 1" $ do
    it "example input 1" $ do
      let input = "{{{}}}"
      part1 input `shouldBe` 6

    it "example input 2" $ do
      let input = "{{{},{},{{}}}}"
      part1 input `shouldBe` 16

    it "example input 3" $ do
      let input = "{{<a!>},{<a!>},{<a!>},{<ab>}}"
      part1 input `shouldBe` 3

    it "real input" $ do
      realInput <- fmap init $ readFile inputFile
      part1 realInput `shouldBe` 17390

  describe "Part 2" $ do
    it "example input 1" $ do
      let input = "<{!>}>"
      part2 input `shouldBe` 2

    it "example input 2" $ do
      let input = "<!!!>>"
      part2 input `shouldBe` 0

    it "example input 3" $ do
      let input = "{<{o\"i!a,<{i<a>}"
      part2 input `shouldBe` 10

    it "real input" $ do
      realInput <- fmap init $ readFile inputFile
      part2 realInput `shouldBe` 7825
