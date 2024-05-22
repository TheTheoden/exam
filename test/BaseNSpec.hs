module BaseNSpec (spec) where

import Test.Hspec
import BaseN 

spec :: Spec
spec = do
  describe "BaseN" $ do
    it "shows the correct representation" $ do
      show (BaseN 10 [1, 2, 3]) `shouldBe` "10: 123"
      show (BaseN 2 [1, 0, 1, 1]) `shouldBe` "2: 1011"

    it "adds two BaseN numbers correctly" $ do
      (BaseN 10 [1, 2, 3]) + (BaseN 10 [4, 5, 6]) `shouldBe` BaseN 10 [5, 7, 9]
      (BaseN 2 [1, 1, 1]) + (BaseN 2 [1, 0, 1]) `shouldBe` BaseN 2 [1, 1, 0, 0]

    it "compares two BaseN numbers correctly" $ do
      (BaseN 10 [1, 2, 3]) `shouldBe` (BaseN 10 [1, 2, 3])
      (BaseN 10 [1, 2, 3]) `shouldSatisfy` (< BaseN 10 [2, 0, 0])
      (BaseN 2 [1, 1, 0]) `shouldSatisfy` (> BaseN 2 [1, 0, 1])
      (BaseN 3 [1, 0]) `shouldSatisfy` (> BaseN 2 [1, 0])
