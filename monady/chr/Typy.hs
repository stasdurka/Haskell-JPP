-- fajny pliczek do wyświetlania czym są _naprawdę_ kombinacje transformatorów monad :)

infixr 5 :->   -- niby strzałka
infix 2 :.:    -- niby przecinek
  
data XType = XType :-> XType     -- (t1 -> t2)
           | XType :.: XType     -- (t1, t2)
           | XEither XType XType -- Either t1 t2
           | XIO XType           -- IO a
           | M XType             -- m a  -- "zmienna" m oznaczająca jakąś monadę
           | R | S | E | C | A   -- "zmienne" oznaczające typy 
           deriving (Show)


-- :k StateT
-- StateT :: * -> (* -> *) -> * -> *
stateT :: XType -> (XType -> XType) -> XType -> XType
-- runStateT :: StateT s m a -> s -> m (a, s)
stateT s m a = s :-> m (a :.: s)


-- runReaderT :: ReaderT r m a -> r -> m a
readerT r m a = r :-> m a

-- runExceptT :: ExceptT e m a -> m (Either e a)
exceptT e m a = m (XEither e a)

-- runIdentity :: Identity a -> a
identity a = a



rsm = readerT R (stateT S M) A

srm = stateT S (readerT R M) A


rse = readerT R (stateT S (exceptT E identity)) A

res = readerT R (exceptT E (stateT S identity)) A

ers = exceptT E (readerT R (stateT S identity)) A


rseio = readerT R (stateT S (exceptT E XIO)) A

ersio = exceptT E (readerT R (stateT S XIO)) A


-- runContT :: ContT r m a -> (a -> m r) -> m r 
contT r m a = (a :-> m r) :-> m r

-- type ESRCIO = ExceptT String (StateT St (ReaderT Env (ContT Ans IO)))

esrcio = exceptT E (stateT S (readerT R (contT C XIO)))



wypisz :: XType -> IO ()
wypisz s = print $ [repl c | c <- show s, c /= ':', c /= 'X'] 
  where repl '.' = ','
        repl c = c
        
main = do
  print "rsm"
  wypisz $ readerT R (stateT S M) A
  print "srm"
  wypisz $ stateT S (readerT R M) A

  print "rse"
  wypisz $ readerT R (stateT S (exceptT E identity)) A
  print "res"
  wypisz $ readerT R (exceptT E (stateT S identity)) A
  print "ers"
  wypisz $ exceptT E (readerT R (stateT S identity)) A

  print "rseio"
  wypisz $ readerT R (stateT S (exceptT E XIO)) A
  print "ersio"
  wypisz $ exceptT E (readerT R (stateT S XIO)) A

  print "esrcio"
  wypisz $ exceptT E (stateT S (readerT R (contT C XIO))) A
  
