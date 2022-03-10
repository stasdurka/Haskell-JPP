permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = [ys ++ [x] ++ zs | ws <- permutations xs, (ys,zs) <- partitions xs]

partitions ::[a] ->[([a],[a])]
partitions [] = [([],[])]
partitions (x:xs) = ([],x:xs) : [(x:ys,zs) | (ys,zs)<-partitions xs]

ones :: [Integer]
ones = 1:ones

nats = 1 : zipWith (+) ones nats
-- 1 1 1 1 1 ...
-- + 1 1 1 1 ...
--   + 1 1 1 ...

inits :: [a] -> [[a]]
inits [] = [[]]
inits (x:xs) = [] : zipWith (:) (repeat x) (inits xs)
inits (x:xs) = [] : map (x:) (inits xs)


