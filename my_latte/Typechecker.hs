{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use isNothing" #-}
import qualified Data.Map as Map
import Data.Maybe(fromJust)
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.Except
import AbsMojeLatte
import Prelude

type Env = Map.Map Name TType
type Name = String

data TType = TInt | TStr | TBool 
            | TFunc [TType] -- funkcje z listy typów zawsze w TInt
            -- lista [TType] to lista typów parametrów funkcji
            -- | Arr Type Integer | Arr2 Type
            deriving (Eq, Ord, Show, Read)

-- data TType = Type |
--   deriving Eq

type RE a = ReaderT Env (ExceptT String Identity) a
runTypeOf :: Env -> RE a -> Either String a
runTypeOf env exp = runIdentity (runExceptT (runReaderT exp env))

findVar :: Name -> RE TType
findVar name = do
    mt <- asks (Map.lookup name)
    case mt of
        Just t -> return t
        Nothing -> throwError ("'"++name++"' not initialized")

typeOf :: Expr -> RE TType
typeOf (ELitInt _) = return TInt
-- typeOf1 (ELam n t0 e1) = do
--   t1 <- local (Map.insert n t0) (typeOf1 e1)
--   return $ t0 :-> t1
typeOf (Elval (EArrEl _ _)) = return TInt
typeOf (Elval (EVar (Ident name))) = findVar name
typeOf ELitTrue = return TBool
typeOf ELitFalse = return TBool
typeOf (EApp (Ident fname) args) = do         -- function application
    t <- asks (Map.lookup fname)              -- types that each arg should have
    case t of
        Nothing -> throwError ("function "++fname++" not declared")
        Just (TFunc argTypes) -> checkArgs fname argTypes args
        _ -> throwError (fname++" is not a function") 
        where 
        checkArgs :: Name -> [TType] -> [Expr] -> RE TType      
        checkArgs fname types exps = do
            case (types, exps) of
                (t:ts,e:es) -> do
                    etype <- typeOf e
                    if etype == t 
                        then checkArgs fname ts es
                        else throwError ("function "++ fname ++": invalid argument type")
                ([],[]) -> return TInt
                ([], e:es) -> throwError ("function "++ fname ++": too many arguments given")
                (t:ts, []) -> throwError ("function "++ fname ++": too few arguments given")

typeOf (EString _) = return TStr
typeOf (Not exp) = do
    t <- typeOf exp
    case t of
        TBool -> return TBool
        _     -> throwError "boolean expression expected"
typeOf (Neg exp) = checkType exp TBool
typeOf (Not exp) = checkType exp TInt
typeOf (EMul exp1 op exp2) = do
    t1 <- checkType exp1 TInt
    t2 <- checkType exp2 TInt
    return TInt
typeOf (EAdd exp1 op exp2) = do
    t1 <- checkType exp1 TInt
    t2 <- checkType exp2 TInt
    return TInt
typeOf (ERel exp1 op exp2) = do
    t1 <- checkType exp1 TBool
    t2 <- checkType exp2 TBool
    return TBool
typeOf (EAnd exp1 op exp2) = do
    t1 <- checkType exp1 TBool
    t2 <- checkType exp2 TBool
    return TBool
typeOf (EOr exp1 op exp2) = do
    t1 <- checkType exp1 TBool
    t2 <- checkType exp2 TBool
    return TBool 

checkType :: Expr -> TType -> RE TType
checkType e t = do
    t' <- typeOf e
    if t == t' then return t
    else throwError ("type " ++ show t ++ " expected instead of " ++ show t')

-- checkProgram :: Program -> RE ()
-- checkProgram = mapM checkTopDef

-- checkTopDef :: TopDef -> RE ()
-- checkTopDef $ FnDef t n args (Block decs stmts) = do

checkBlock :: Block -> RE ()
checkBlock $ Block [] stmts = do
    mapM checkStmt stmts
checkBlock $ Block ((Decl t item):ds) stmts = do
    case item of
        NoInit (Ident name) -> local (M.insert name t) checkBlock (Block ds stmts)
        Init (Ident name expr) -> do
            t' <- checkType expr t          -- throws error if wrong type
            local (M.insert name t) checkBlock (Block ds stmts)

-- declare :: Decl -> RE ()
-- declare (Decl t (i:is)) = do
--     case i of
--         NoInit (Ident name) -> local (M.insert name t) declare (Decl t is)
--         Init (Ident name) -> local (M.insert name t) declare (Decl t is)
-- declare (Decl t [])


checkStmt :: Stmt -> RE ()
checkStmt Empty = return ()
checkStmt BStmt b = checkBlock b
checkStmt Ass lval expr =
    case lval of 
        EVar (Ident name) -> do
            t <- asks (M.lookup name)
            t' <- typeOf expr
        EArrEl (Ident name) expr -> 

-- always returns TInt if the arguments match their expected types,
-- and throws an error otherwise


  -- return $ fromJust t
-- typeOf (EApp e1 e2) = do
--   tlambda <- typeOf e1
--   tx <- typeOf e2
--   case tlambda of 
--     tx :-> tret -> return tret
--     t -> throwError ("Cannot apply "++show e1++"::"++show t++" to "++show e2)

-- runTypeOf Map.empty (typeOf (EVar "a"))