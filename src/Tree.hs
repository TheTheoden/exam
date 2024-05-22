module Tree (Tree(..), inOrder, preOrder, postOrder) where

data Tree a = Leaf | Node (Tree a) a (Tree a)

instance Foldable Tree where
    foldMap _ Leaf = mempty
    foldMap f (Node left x right) = foldMap f left `mappend` f x `mappend` foldMap f right

inOrder :: Tree a -> [a]
inOrder Leaf = []
inOrder (Node left x right) = inOrder left ++ [x] ++ inOrder right

preOrder :: Tree a -> [a]
preOrder Leaf = []
preOrder (Node left x right) = [x] ++ preOrder left ++ preOrder right

postOrder :: Tree a -> [a]
postOrder Leaf = []
postOrder (Node left x right) = postOrder left ++ postOrder right ++ [x]
