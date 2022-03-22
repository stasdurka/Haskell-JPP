-- 14 marca (poniedziałek) - drzewa

data Tree a = Empty 
    | Node a (Tree a) (Tree a) 

-- 1a. a. stwórz własne instancje Eq, Show

instance Show a => Show (Tree a) where
   show t = -- ...

instance Eq a => Eq (Tree a) where
   t1 == t2 = -- ...

-- 1b. napisz funkcje toList :: Tree costam

Node a l p = toList l ++ [a] ++ toList p -- moze byc kwadratowe 
-- lepiej: 
dopisz :: Tree a -> [a] -> [a]
toList = dopisz t [] 

-- 1c. zaimplementuj drzewo bst z funkcjami

insert :: (Ord a) => a -> Tree a -> Tree a
member :: (Ord a) => a -> Tree a -> Bool
fromList :: (Ord a) => [a] -> Tree a

Node a l p -> shows l . shows a . shows p