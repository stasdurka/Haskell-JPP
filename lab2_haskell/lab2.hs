-- 14 marca (poniedziałek) - drzewa
data Tree a = Empty 
   | Node a (Tree a) (Tree a) 

-- 1 a. stwórz własne instancje Eq, Show
instance Show a => Show (Tree a) where
   show Empty = "" 
   -- show (Node x l r) = " (Node " ++ (show x)  ++ (show l) ++ (show r) ++ ")"
   -- drugi show - zwraca listę węzłów w obiegu infiksowym jako String:
   show t = let {
      g :: Show a => (Tree a) -> ShowS; -- type ShowS = String -> String
      g Empty = showString "";
      g (Node a l r) = shows l . shows a . shows r;
   } in g t ""
      
 

instance Eq a => Eq (Tree a) where
   Empty == Empty = True
   (Node v l r) == (Node v' l' r') = (v==v')&&(l==l')&&(r==r') 

-- 1 b. napisz funkcje toList :: Tree a -> [a]
-- ktora zamieni drzewo w liste elementow drzewa (w porzadku infiksowym)

toList :: Tree a -> [a]
toList Empty = []
toList (Node a l p) = toList l ++ [a] ++ toList p -- <- o(n^2) przez konkatenację?
-- lepiej: 
-- dopisz :: Tree a -> [a] -> [a]
-- dopisz Empty xs = xs
-- dopisz (Node a l p) xs = dopisz l (dopisz a (dopisz p xs))
-- toList = dopisz t [] 

-- 1 c. zaimplementuj drzewo bst z funkcjami:
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
fromList (x:xs) = Node x (fromList [y | y<-xs, y < x]) (fromList [z | z<-xs, z > x])

-- Node a l p -> shows l . shows a . shows p

-- 1 d. Stwórz moduł drzew BST i wykorzystaj go w innym module do sortowania [Int]
-- => w plikach BST.hs is 

-- 2. Zdefniować data MyMaybe a = MyNothing | MyJust a oraz showsPrec dla niego 
-- tak aby nawiasy były tam gdzie trzeba
-- .......

-- 3. ...

-- 4. Napisz funkcje
elimMaybe :: c -> (a -> c) -> Maybe a -> c
elimMaybe c f Nothing = c
elimMaybe _ f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
elimMaybe y Nothing = y
elimMaybe _ (Just x) = x

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing = Nothing
mapMaybe f (Just x) = f x

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead x:xs = Just x

elimEither :: (a -> c) -> (b -> c) -> Either a b -> c
elimEither f g (Left a) = f a
elimEither f g (Right b) = f b

mapEither :: (a1 -> a2) -> (b1 -> b2) -> Either a1 b1 -> Either a2 b2
mapEither f g (Left x) = Left (f x)
mapEither f g (Right y) = Right (g y)

mapRight ::  (b1 -> b2) -> Either a b1 -> Either a b2
mapRight f (Left a) = Left a
mapRight f (Right b) = Right (f b)

fromEither :: Either a a -> a
fromEither (Right a) = a
fromEither (Left a) = a