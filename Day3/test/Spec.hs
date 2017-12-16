import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
  describe "Part 1" $ do
    it "distance of 23" $ do
      part1 23 `shouldBe` 2
    it "distance of problem input" $ do
      part1 347991 `shouldBe` 480
