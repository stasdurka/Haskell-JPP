
-- Ten plik się nie kompiluje! To jest tylko plik poglądowy. 
-- Te wszystkie rzeczy poniżej są w:
import Control.Monad.State

-- specyfikacja protokołu MonadState
-- s to typ stanu, np (np Map String Int)
class Monad m => MonadState s m | m -> s where
  -- odczytuje stan
  get :: m s
  -- zapisuje stan
  put :: s -> m ()

-- użyteczne funkcje pomocnicze
-- odczytaj "fragment" stanu
gets :: MonadState s m => (s -> a) -> m a
gets f = do
  s <- get
  return (f s)

-- zmodyfikuj stan
modify :: MonadState s m => (s -> s) -> m ()
modify f = do
  s <- get
  put (f s)


-- implementacja za pomocą konstruktora typu
-- State s izomorficznego z "λa. s -> (a,s)"
newtype State s a = State {runState :: s -> (a, s)}

instance Monad (State s) where
   -- return :: a -> State s a
   return a = State $ \s -> (a,s)
   -- (>>=) :: State s a -> (a -> State s b) -> State s b
   (State f) >>= g = State $ \s -> let (a, s') = runState f s in
                             runState (g a) s'

-- funkcje uruchamiające
runState  :: State s a -> s -> (a, s)  -- zdefiniowane powyżej

  -- jak potrzebujemy samą wartość
evalState :: State s a -> s -> a
evalState st = fst . runState st

  -- jak potrzebujemy tylko stan końcowy
execState :: State s a -> s  ->  s
execState st = snd . runState st



instance MonadState s (State s) where
  get = State $ \s -> (s, s)
  put s = State $ \_ -> ((), s)

-- i teraz mamy:

get :: State s s
put :: s -> State s () 

gets :: (s -> a) -> State s a
gets f == get >>= (\s -> return (f s))
       == State $ \s -> let (a, s') = runState get s in
                              runState ((\s -> return (f s)) a) s'
       == State $ \s -> let (a, s') = runState get s in
                              runState (return (f a)) s'
       == State $ \s -> let (a, s') = runState (State $ \s'' -> (s'', s'')) x in
                              runState (State $ \s'' -> (f a, s'')) s'
       == State $ \s -> let (a, s') = (\s'' -> (s'', s'')) s in
                              (\s'' -> (f a, s'')) s'
       == State $ \s -> let (a, s') = (s, s) in
                              (f a, s')
       == State $ \s -> (f s, s)


modify :: (s -> s) -> State s ()
modify f == get >>= (\s -> put (f s))
         == State $ \s -> let (a, s') = runState (State $ \s' -> (s', s')) s in
                                runState ((\s -> put (f s)) a) s'
         == State $ \s -> let (a, s') = (\s' -> (s', s')) s in
                                runState (put (f a)) s'
         == State $ \s -> let (a, s') = (s, s) in
                                runState (State $ \_ -> ((),f a)) s'
         == State $ \s -> runState (State $ \_ -> ((),f s)) s
         == State $ \s -> (\_ -> ((),f s)) s
         == State $ \s -> ((),f s)





