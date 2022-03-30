module BST where
import Tree

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty
insert x (Node v l r)
    | x < v = Node v (insert x l) r
    | otherwise = Node v l (insert x r)

member :: (Ord a) => a -> Tree a -> Bool
member x Empty = False
member x (Node v l r)
    | x == v = True
    | x < v = member x l
    | x > v = member x r

fromList :: (Ord a) => [a] -> Tree a -- dowolna lista
fromList [] = Empty
fromList (x:xs) = Node x (fromList [y | y<-xs, y < x]) (fromList [z | z<-xs, z >= x])
