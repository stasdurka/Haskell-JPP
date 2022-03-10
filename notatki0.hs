set = [3 * x | x <-[1..10], mod x 2 == 1]
set2 = [(x,y) | x<-[1..5], y<-[1,2,3], x+y == 5, x*y == 6]

fact1 n = if n<2 then 1 else n*fact1(n-1)

fact2 n | n <= 1 = 1
        | otherwise = n*fact2(n-1)

fact3 0 = 1
fact3 n = n * fact3 (n-1)

fact4 n = case n of {
    0 -> 1;
    m -> m * fact4(m-1)
}

myzip :: [a] -> [b] ->[(a,b)]
myzip (x:xs) (y:ys) = (x,y):myzip xs ys
myzip _ _ = []

-- let 
--     x = 1
--     y = 2
-- in x+y

-- let answer = 42 in answer


f xs = let
        len [] = 0
        len (x:xs) = 1 + len xs
    in len xs

-- zadanie: podzielic liste na dwie: el. <= n i el. > n
splitBy :: Int -> [Int] -> ([Int],[Int])
splitBy n [] = ([],[])
splitBy n (x:xs) = let (ys, zs) = splitBy n xs in
    if x<=n then (x:ys,zs) else (ys,x:zs)

splitBy' n (x:xs)
    | x <= n = let (ys,zs) = splitBy' n xs in (x:ys, zs)
    | x > n  = let (ys,zs) = splitBy' n xs in (ys, x:zs)

splitBy'' n [] = ([], [])
splitBy'' n (x:xs)
    | x <= n = (x:ys,zs)
    | otherwise  = (ys,x:zs)
    where (ys,zs) = splitBy'' n xs

countdown :: Int -> [Int]
countdown n
    | n < 0 = []
    | otherwise = let xs=countdown (n-1) in n:xs

collatz :: Int -> [Int]
collatz n
    | n == 0 = [0]
    | n == 1 = [1]
    | otherwise = let m = (if mod n 2 == 0 then div n 2 else 3*n+ 1) 
        in let l = collatz m in n:l
    -- where m = (if mod n 2 == 0 then div n 2 else 3*n+ 1)

-- zadania po wykładzie:

-- 1. napisz własne odpowiedniki std funkcji
head' :: [a] -> a
head' (x:xs) = x
tail' (x:xs) = xs

take' :: Int -> [a] -> [a]
take' n [] = []
take' 0 (xs) = []
take' n (x:xs) = x:ys where ys = take' (n-1) xs

drop' :: Int -> [a] -> [a]
drop' n [] = []
drop' 0 xs = xs
drop' n (x:xs) = drop' (n-1) xs

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
-- map' f (x:xs) = let y = f x in 
--                let ys = map' f xs in
--                (y:ys)
-- map' f (x:xs) = let (y,ys) = (f x, map' f xs) in
--                (y:ys)
map' f (x:xs) = (f x):(map' f xs)

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = let ys = concat' xs in x++ys

-- 2. Napisz funkcję inits, ktora dla danej listy da listę wszystkich jej odcinków początkowych, np.
-- inits [1,2] == [[],[1],[1,2]]
-- inits [1..3] [[],[1],[1,2],[1,2,3]]
-- ????
inits :: [a] -> [[a]]
inits [] = [[]]
inits l = inits' [] l 

inits' :: [[a]] -> [a] -> [[a]]
inits' [] (x:xs) = [[x]] ++ (inits xs)
inits' l (x:xs) = l++[(last l)++[x]] ++ (inits xs)

-- 3.partitions: dla xs daje liste wszystkich par (ys,zs) takich, ze:
-- xs == ys ++ zs

partitions :: [a] -> [([a],[a])]
partitions [] = [([],[])] -- KLUCZOWE, zeby dzialalo zad. 4
partitions xs = part [] xs

part :: [a] -> [a] -> [([a],[a])]
part l1 [] = [(l1,[])]
part l1 (x:l2) = [(l1, x:l2)] ++ part (l1++[x]) l2

-- 4. Napisz funkcje permutations, ktora dla danej listy da listę wszystkich jej permutacji 
-- (dla unikniecia niejasności mozemy założyć, ze wszystkie elementy listy wejściowej sa różne)

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = let perms = permutations xs -- lista permutacji xs
                    in let p = map partitions perms -- dla kazdej permutacji, tworzymy liste wszystkich mozliwych partycji
                    in let p' = concat p
                    in map (insert x) p'
-- *Main> perms
-- [[1,2],[2,1]]
-- *Main> map partitions perms
-- [[([],[1,2]),([1],[2]),([1,2],[])],[([],[2,1]),([2],[1]),([2,1],[])]]
insert :: a -> ([a],[a]) -> [a]
insert x (l,r) = l++[x]++r

-- 5. cwiczymy funkcje wyzszego rzedu 
-- a. Napisz funkcje
-- incAll :: [[Int]] -> [[Int]]
-- która zwiększy o 1 każdy element każdego elementu swojego argumentu, np
-- *Main Data.List> incAll $ inits [1..3] [[],[2],[2,3],[2,3,4]]

incAll :: [[Int]] -> [[Int]]
incAll xs = map (map (+1)) xs

-- b. Napisz przy pomocy foldr
--     silnię
fact :: Int -> Int
fact1 n = foldr (*) 1 [1..n]
--     concat :: [[a]] -> [a]
concat :: [[a]] -> [a]
concat xs = foldr 
-- c. Napisz nub przy pomocy filter