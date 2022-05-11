-- notatki z wykładu - Monady 1

class Monad obliczenie where
    return :: a -> obliczenie a
    (>>=) :: obliczenie a -> (a -> obliczenie b) -> obliczenie b

(>>=) :: Monad m => m a -> (a -> m b) -> m b

o1 >> o2 = o1 >>= \_ -> o2

-- 0. Identity Monad
instance Monad Identity where
    return a            = Identity a    -- return = id
    (Identity x) >>= f  = f x

-- 1. Maybe Monad
-- return :: a -> Maybe a
-- (>>=) ::Maybe a -> (a -> Maybe b) -> Maybe b
instance Monad Maybe where      -- k = kontynuacja = (a -> Maybe b) 
    return x      = Just x
    Nothing >>= k = Nothing 
    Just x  >>= k = k x

-- bez monad
case obliczenie1 of
    Nothing -> Nothing
    Just x -> case obliczenie2 of
                Nothing -> Nothing
                Just y -> obliczenie3

data Exp = Val Int | Div Exp Exp

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (div x y)

-- monada Maybe:
-- mx == Maybe x
-- f :: a -> Maybe a
mx >>= f = case mx of 
    Nothing -> Nothing
    Just x  -> f x

eval :: Exp -> Maybe Int
eval (Val n) = Just n
eval (Div x y) = eval x >>= (\n ->
                 eval y >>= (\m ->
                 safediv n m))

-- używając do i return:
 eval (Val n) = return n    -- Just n
 eval (Div x y) = do
                    n <- eval x
                    m <- eval y
                    safediv n m

-- 3. Either Monad
instance Monad (Either error) where
    return = Right
    (Left e)  >>= _ = Left e
    (Right x) >>= k = k x


-- > Left "error" >> return 3
-- Left "error"

-- > do { n <- readEither "41" ; return (n+1) }
-- Right 42