module RotateSpec (spec) where

import Test.Hspec
import Rotate (rotate)

spec :: Spec
spec = do
  describe "rotate" $ do
    it "rotates the list to the left" $ do
      rotate 2 "abcdef" `shouldBe` "cdefab"
      rotate 3 [1, 2, 3, 4, 5] `shouldBe` [4, 5, 1, 2, 3]

    it "rotates the list to the right" $ do
      rotate (-2) "abcdef" `shouldBe` "efabcd"
      rotate (-3) [1, 2, 3, 4, 5] `shouldBe` [3, 4, 5, 1, 2]
