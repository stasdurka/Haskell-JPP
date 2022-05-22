import qualified Data.Map as Map
import Data.Maybe(fromJust)
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.Except



import IntLambda
type Env = Map.Map Name Type

typeOf0 :: Env -> Exp -> Type
typeOf0 env (EInt _) = TInt
typeOf0 env (ELam n t0 e1) = t0 :-> t1 where 
  t1 = typeOf0 env' e1
  env' = Map.insert n t0 env
typeOf0 env (EVar n) = fromJust $ Map.lookup n env
typeOf0 env (EApp e1 e2) = 
  case typeOf0 env e1 of
    t2 :-> t0 | t2 == typeOf0 env e2 -> t0
    t -> error ("Cannot apply "++show e1++"::"++show t++" to "++show e2)

type RE a = ReaderT Env (ExceptT String Identity) a
runTypeOf :: Env -> RE a -> Either String a
runTypeOf env exp = runIdentity (runExceptT (runReaderT exp env))

typeOf1 :: Exp -> RE Type
typeOf1 (EInt _) = return TInt
typeOf1 (ELam n t0 e1) = do
  t1 <- local (Map.insert n t0) (typeOf1 e1)
  return $ t0 :-> t1
typeOf1 (EVar n) = do
  maybet <- asks (Map.lookup n)
  case maybet of
    Nothing -> throwError ("variable '"++n++"' not initialized")
    Just t -> return t
  -- return $ fromJust t
typeOf1 (EApp e1 e2) = do
  tlambda <- typeOf1 e1
  tx <- typeOf1 e2
  case tlambda of 
    tx :-> tret -> return tret
    t -> throwError ("Cannot apply "++show e1++"::"++show t++" to "++show e2)

typeOf :: Exp -> Type
typeOf = typeOf0 Map.empty

-- runTypeOf :: Exp -> Either String Type
-- runTypeOf exp = runReader (typeOf1 exp) Map.empty
