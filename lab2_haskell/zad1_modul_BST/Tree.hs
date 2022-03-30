module Tree where

data Tree a = Empty 
   | Node a (Tree a) (Tree a) 

instance Show a => Show (Tree a) where
    show Empty = " *" 
    show (Node x l r) = " (Node " ++ (show x)  ++ (show l) ++ (show r) ++ ")"

instance Eq a => Eq (Tree a) where
    Empty == Empty = True
    (Node v l r) == (Node v' l' r') = (v==v')&&(l==l')&&(r==r') 

-- zamienia drzewo w liste elementow drzewa (w porzadku infiksowym)
toList :: Tree a -> [a]
toList (Node a l p) = toList l ++ [a] ++ toList p
toList Empty = []