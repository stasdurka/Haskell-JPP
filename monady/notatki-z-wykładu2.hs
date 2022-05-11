-- notatki z wykładu 2
-- class Monad m where
--     return :: a -> m a
--     (>>=) :: m a -> (a -> m b) -> m b

-- class (Monad m) => MonadReader r m | m -> r where
    -- ask :: m r
    -- local :: (r -> r) -> m a -> m a
    -- asks :: (MonadReader r m) => (r -> a) -> m a

import Control.Monad.Reader

data Env = Env {var_a :: Int, var_b :: Int}
type R a = Env -> a -- czyli R to jest ta MONADA "m", a r == Env
-- instance (MonadReader Env) R
example :: R Int -- R Int = R -> Int
example = do
    a <- asks var_a
    b <- asks var_b
    return (a+b)

run :: R a -> a
run r = r Env {var_a = 5, var_b = 10}
-- run example

-- type Obliczenie wynik = (Stan -> (wynik, Stan))
-- return :: a -> Obliczenie a
-- return x s = (x,s)
-- (>==) :: Obliczenie a -> (a -> Obliczenie b) -> Obliczenie b)
-- (o >>= k) s = let (x,s') = o s in (k x) s' -- k x = Obliczenie b == funkcja ze stanu w (wynik,stan) => (k x) s' = (wynik, stan)