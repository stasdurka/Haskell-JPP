-- readInts2 :: String -> Either String [Int]
-- sumInts :: String -> String

-- readInts :: String -> [Int]
-- *Main> readInts "1 23 456 7.8 abc 9"
-- [1,23,456,9]

data Exp = Val Int | Div Exp Exp | Add Exp Exp
safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (div x y)

-- Maybe monad:
-- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
-- instance Monad Maybe where
--     return x = Just x
--     Nothing >>= _ = Nothing
--     Just x >>= f = f x

eval :: Exp -> Maybe Int
eval (Val x) = return x     -- Just n
eval (Div x y) = eval x >>= (\n -> 
                 eval y >>= (\m ->
                 safediv n m))
-- eval (Div x y) = do {
--                     n <- eval x;
--                     m <- eval y;
--                     safediv n m;
--                 }
eval (Add x y) = do
                    n <- eval x
                    m <- eval y
                    return (n + m)
-- eval (Add x y) = eval x >>= \n ->
--                  eval y >>= \m ->
--                  return (n+m)
-- itd..

-- b. Napisz funkcje obliczające wartość listy wyrażeń:
evalList' :: [Exp] -> [Maybe Int]
evalList' l = fmap eval l

-- (>>=) :: Maybe a -> (a -> Maybe [a]) -> Maybe [a]
evalList :: [Exp] -> Maybe [Int]
evalList l = mapM eval l

