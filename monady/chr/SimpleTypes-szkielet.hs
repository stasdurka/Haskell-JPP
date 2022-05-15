{-# LANGUAGE FlexibleContexts #-}
import qualified Data.Map as Map
import Data.Maybe(fromJust)
import Control.Monad.Reader
import Control.Monad.Except

import IntLambda
type Env = Map.Map Name Type
type TCM a = ExceptT String (Reader Env) a

{-

 ExceptT e m a 

e - error type
m - inner monad 
a - value type

runExceptT :: (ExceptT e m a) -> m (Either e a)


runExceptT :: TCM a -> Reader Env (Either String a)

(MonadReader r m) => MonadReader r (ExceptT e m)
(Monad m) => MonadError e (ExceptT e m)

throwError :: e -> m a
catchError :: m a -> (e -> m a) -> m a

-}


-- do napisania samodzielnie
typeOf0 :: Exp -> TCM Type




-- gotowe do użycia
runEnvR r = runReader r  Map.empty

runTCM :: TCM a -> Either String a
runTCM = runEnvR . runExceptT

typeOf1 :: Exp -> TCM Type
typeOf1 exp = catchError (typeOf0 exp) handler where
  handler e = throwError $ 
    "Type error in\n"
    ++show exp++"\n"
    ++e

typeOf :: Exp -> Either String Type    
typeOf exp = runTCM (typeOf1 exp)

typeCheck :: Exp -> IO ()
typeCheck exp = case typeOf exp of
                  Left e -> putStrLn "Error: " >> putStrLn e
                  Right t -> print t
