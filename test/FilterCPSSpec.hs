module FilterCPSSpec (spec) where

import Test.Hspec
import FilterCPS

spec :: Spec
spec = do
  describe "filterCPS" $ do
    it "filters out odd numbers" $ do
      filterCPS even [1, 2, 3, 4, 5] (\result -> result `shouldBe` [2, 4])

    it "filters out odd numbers and applies" $ do
      filterCPS even [1, 2, 3, 4, 5] (\result -> result `shouldBe` [2, 4])

    it "filters out strings with length greater than 3" $ do
      filterCPS (\x -> length x <= 3) ["apple", "banana", "orange", "wow"] (\result -> result `shouldBe` ["wow"])