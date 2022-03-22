-- 1. a..c - bylo w lab0.hs
-- d. Napisz funkcję obliczającą iloczyn skalarny dwóch list liczb; użyj zipWith
scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = let l = zipWith (*) xs ys in foldr (+) 0 l

-- 2. Napisz funkcję triples :: Int -> [(Int,Int,Int)], która dla argumentu n da listę wszystkich trójek liczb laturalnych o elementach z [1..n]
triples :: Int -> [(Int,Int,Int)]
triples n = [(x,y,z) | x<-[1..n], y<-[1..n], z<-[1..n], x <= y, y <= z]
-- 3. Napisz funkcję triads :: Int -> [(Int,Int,Int)], ktora da liste trojek pitagorejskich (x^2+y^2=z^2 dla x,y,z <= n)
triads :: Int -> [(Int,Int,Int)]
-- triads n [] = []
triads n = [(x,y,z) | x<-[1..n], y<-[1..n], z<-[1..n], (x*x + y*y) == z*z ]
-- b. skoro (3,4,5) juz jest na liscie to (6,8,10) i (12,16,20) sa "trywialne" - nie zawieramy ich
triads' n = [(x,y,z) | x<-[1..n], y<-[1..n], z<-[1..n], (x*x + y*y) == z*z ] -- ???!!*** jak to zrobic hmm?

-- 4.
-- data Maybe a = Nothing | Just a
-- czyli jesli x::a to Just x :: Maybe a

-- size :: Maybe a -> Int
-- size Nothing = 0
-- size (Just _) = 1

-- a.
incMaybe :: Maybe Int -> Maybe Int 
incMaybe Nothing = Nothing 
incMaybe (Just x) = Just (x+1)
-- b.
addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe Nothing _ = Nothing 
addMaybe _ Nothing = Nothing
addMaybe (Just x) (Just y) = Just (x+y)

--6a. Napisz funkcję
-- indexOf :: Char -> String -> Maybe Int
-- taką, że jeśli c wystepuje w s na pozycji n, to (indexOf c s) == Just n, wpp Nothing, np.
-- *Main> indexOf 'a' "Ala"
-- Just 2
-- *Main> indexOf 'b' "Ala"
-- Nothing
indexOf :: Char -> String -> Maybe Int
indexOf = let
    idx :: Int -> Char -> String -> Maybe Int
    idx n c (x:xs) = if c==x then Just n else idx (n+1) c xs
    idx n c [] = Nothing
    in idx 0
-- 6b. Napisz funkcje
-- positions :: Char -> String -> [Int]
-- taką, że (positions c s) daje listę wszystkich pozycji, na których c wystepuje w s, np.
-- *Main> positions 'a' "Ala ma kota"
-- [2,5,10]
-- *Main> positions 'b' "Ala ma kota"
-- []

positions :: Char -> String -> [Int]
positions c str =
    let
        pos :: Int -> Char -> String -> [Int]
        pos n c [] = []
        pos n c (x:xs)
            | x == c = n:l
            | otherwise = l
            where l = pos (n+1) c xs
    in pos 0 c str







