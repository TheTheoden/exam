module TreeSpec (spec) where

import Test.Hspec
import Tree (Tree(..), inOrder, preOrder, postOrder)

spec :: Spec
spec = do
  let tree = Node (Node Leaf 1 (Node Leaf 2 Leaf)) 3 (Node Leaf 4 Leaf)

  describe "Tree traversals" $ do
    it "performs in-order traversal" $ do
      inOrder tree `shouldBe` [1, 2, 3, 4]

    it "performs pre-order traversal" $ do
      preOrder tree `shouldBe` [3, 1, 2, 4]

    it "performs post-order traversal" $ do
      postOrder tree `shouldBe` [2, 1, 4, 3]
