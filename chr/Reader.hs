-- Ten plik się nie kompiluje! To jest tylko plik poglądowy. 
-- Te wszystkie rzeczy poniżej są w:
import Control.Monad.Reader


-- specyfikacja protokołu MonadReader
-- r to typ środowiska (np Map String Int)
class Monad m => MonadReader r m | m -> r where
   ask :: m r 
   local :: (r -> r) -> m a -> m a

-- użyteczna funkcja pomocnicza
asks ::  (MonadReader r m) => (r -> a) -> m a
asks h = do
  r <- ask
  return $ h r


-- implementacja protokołu za pomocą konstruktora typu "λa. r -> a"

instance Monad ((->) r) where       -- "(->) r" to tak naprawdę  (r -> ), coś jak (3-)  
   -- return :: a -> (r -> a)
   return a = \r -> a
   -- >>= :: (r -> a) -> (a -> (r -> b)) -> (r -> b)
   g >>= f = (\r -> f (g r) r)


instance MonadReader r ((->) r) where
  -- ask :: r -> r
  ask = \r -> r
  -- local :: (r -> r) -> (r -> a) -> (r -> a)
  local f g = \r -> g (f r)

-- teraz asks to oczywiście:
asks :: (r -> a) -> (r -> a)
asks h == ask >>= (\r -> return $ h r)
       == \r -> (\r' -> return $ h r') (ask r) r
       == \r -> (\r' r'' -> h r') (ask r) r
       == \r -> (\r'' -> h (ask r)) r
       == \r -> h (ask r)
       == \r -> h ((\r' -> r') r)
       == \r -> h r
       == h


-- implementacja za pomocą typu "Reader env"
-- izomorficznego z "λa. env -> a"
newtype Reader env a = Reader {runReader :: env -> a}
--runReader :: Reader env a -> env -> a

instance Monad (Reader env) where
   -- return :: a -> Reader env a
   return a = Reader (\_ -> a)
   -- >>= :: Reader env a -> (a -> Reader env b) -> Reader env b
   (Reader f) >>= g = Reader $ \e -> runReader (g (f e)) e

instance MonadReader env (Reader env) where
  -- ask :: Reader env env
  ask = Reader $ \e -> e
  -- local :: (env -> env) -> Reader env a -> Reader env a
  local f g = Reader $ \e -> runReader g (f e)

