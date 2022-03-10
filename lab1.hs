-- 1. a Napisz za pomoca map funkcje
-- incAll :: [[Int]] -> [[Int]]
-- która zwiększy o 1 każdy element każdego elementu swojego argumentu, np
-- *Main Data.List> incAll $ inits [1..3] [[],[2],[2,3],[2,3,4]]

incAll :: [[Int]] -> [[Int]]
incAll xs = map (map (+1)) xs

-- 1 b. Napisz przy pomocy foldr
--     silnię
fact :: Int -> Int
fact n = foldr (*) 1 [1..n]
--     concat :: [[a]] -> [a]
concat'' :: [[a]] -> [a]
concat'' = foldr (++) []

-- c. Napisz nub (wywala duplikaty z listy) przy pomocy filter
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : (filter (/=x) (nub xs))
-- nub xs = foldr (\ x -> filter (/= x)) [] xs

-- d. Napisz funkcję obliczającą iloczyn skalarny dwóch list liczb; użyj zipWith
scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = let l = zipWith (*) xs ys in foldr (+) 0 l

-- 2. Napisz funkcję triples :: Int -> [(Int,Int,Int)], która dla argumentu n da listę wszystkich trójek liczb laturalnych o elementach z [1..n]
triples :: Int -> [(Int,Int,Int)]
triples n = [(x,y,z) | x<-[1..n], y<-[1..n], z<-[1..n]]
-- 3. Napisz funkcję triads :: Int -> [(Int,Int,Int)], ktora da liste trojek pitagorejskich (x^2+y^2=z^2 dla x,y,z <= n)
triads :: Int -> [(Int,Int,Int)]
-- triads n [] = []
triads n = [(x,y,z) | x<-[1..n], y<-[1..n], z<-[1..n], (x*x + y*y) == z*z ]