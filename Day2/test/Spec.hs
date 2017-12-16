import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
  describe "Part 1" $ do
    it "example 1" $ do
      part1 "5 1 9 5" `shouldBe` 8

    it "example 2" $ do
      part1 "1001 3 0 12 5 1 9 5" `shouldBe` 1001

    it "problem input" $ do
      content <- fmap init $ readFile "test/input.txt"
      part1 content `shouldBe` 47136

  describe "Part 2" $ do
    it "example 1" $ do
      part2 "5 9 2 8" `shouldBe` 4

    it "example 2" $ do
      part2 "9 4 7 3" `shouldBe` 3

    it "problem input" $ do
      content <- fmap init $ readFile "test/input.txt"
      part2 content `shouldBe` 250

