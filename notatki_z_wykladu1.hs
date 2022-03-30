-- quicksort 1
q [] = []
q (x:xs) = q ys++[x]++q zs
where
    ys = [a | a <- xs, a <= x]
    zs = [b | b <- xs, b > x]

-- quicksort 2
qs [] = []
qs (x:xs) = (qs (filter (<=x) xs))
            ++ [x]
            ++ (qs (filter (>x) xs))
    
-- ale mozna inaczej:
partition :: (a->Bool) -> [a] ->([a],[a])
partition p [] = ([],[])
partition p (x:xs)
    | p x = (x:ys, zs) -- "p x =..." <=> (p x == true) = ...
    | otherwise = (ys, x:zs)
    where (ys,zs) = partition p xs
