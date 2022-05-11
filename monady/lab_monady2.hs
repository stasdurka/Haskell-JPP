data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)
renumber :: Tree a -> Tree Int
renumber t = renumber' 0 t
renumber' :: Int -> Tree a -> Tree Int
renumber' h (Node x l r) = Node h (renumber' (h+1) l) (renumber' (h+1) r)
renumber' _ Empty = Empty