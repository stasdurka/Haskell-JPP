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

insert :: a -> ([a],[a]) -> [a]
insert x (l,r) = l++[x]++r
-- *Main> perms
-- [[1,2],[2,1]]
-- *Main> map partitions perms
-- [[([],[1,2]),([1],[2]),([1,2],[])],[([],[2,1]),([2],[1]),([2,1],[])]]