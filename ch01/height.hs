data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Show)

leftChild :: Tree a -> Tree a
leftChild Empty = Empty
leftChild (Node _ l _) = l

rightChild :: Tree a -> Tree a
rightChild Empty = Empty
rightChild (Node _ _ r) = r

height :: Tree a -> Integer
height Empty = 0
height t = max ((height . rightChild) t) ((height . leftChild) t) + 1
