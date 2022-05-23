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

-- data TType = TInt | TStr | TBool 
--             | TFunc [TType] -- funkcje z listy typów zawsze w TInt
--             -- lista [TType] to lista typów parametrów funkcji
--             -- | Arr Type Integer | Arr2 Type
--             deriving (Eq, Ord, Show, Read)

data TType = T Type | TFunc [TType]
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

checkType :: Expr -> TType -> RE TType
checkType e t = do
    t' <- typeOf e
    if t == t' then return t
    else throwError ("type " ++ show t ++ " expected instead of " ++ show t')

compareTypes :: TType -> TType -> RE TType
compareTypes t t' = do
    if t == t' then return t
    else throwError ("type " ++ show t ++ " expected instead of " ++ show t')

typeOf :: Expr -> RE TType
typeOf (ELitInt _) = return $ T Int
-- typeOf1 (ELam n t0 e1) = do
--   t1 <- local (Map.insert n t0) (typeOf1 e1)
--   return $ t0 :-> t1
-- typeOf (Elval (EArrEl _ _)) = return TInt
typeOf (Elval (EVar (Ident name))) = findVar name
typeOf ELitTrue = return (T Bool)
typeOf ELitFalse = return (T Bool)
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
                ([],[]) -> return (T Int)
                ([], e:es) -> throwError ("function "++ fname ++": too many arguments given")
                (t:ts, []) -> throwError ("function "++ fname ++": too few arguments given")

typeOf (EString _) = return (T Str)
typeOf (Neg exp) = checkType exp (T Bool)
typeOf (Not exp) = checkType exp (T Int)
typeOf (EMul exp1 op exp2) = do
    t1 <- checkType exp1 (T Int)
    t2 <- checkType exp2 (T Int)
    return $ T Int
typeOf (EAdd exp1 op exp2) = do
    t1 <- checkType exp1 (T Int)
    t2 <- checkType exp2 (T Int)
    return (T Int)
typeOf (ERel exp1 op exp2) = do
    t1 <- checkType exp1 (T Bool)
    t2 <- checkType exp2 (T Bool)
    return $ T Bool
typeOf (EAnd exp1 exp2) = do
    t1 <- checkType exp1 (T Bool)
    t2 <- checkType exp2 (T Bool)
    return $ T Bool
typeOf (EOr exp1 exp2) = do
    t1 <- checkType exp1 (T Bool)
    t2 <- checkType exp2 (T Bool)
    return (T Bool)

-- checkTopDef :: TopDef -> RE ()
-- checkTopDef (FnDef t n args b) = do

-- checks function body and declaration and
-- returns a list of types of the parameters
-- assures it returns an Int
checkFunction :: TopDef -> RE ()
checkFunction (FnDef t name [] b) = checkBlock b    
checkFunction (FnDef t name (args@(Arg t' (Ident argname)):as) b) = do
    compareTypes (T t) (T Int)      -- throws error
    local (Map.insert argname (T t)) (checkFunction (FnDef t name as b))
    return ()

checkProgram :: Program -> RE ()
checkProgram (Program (f@(FnDef t (Ident name) args b):fs)) = do
    checkFunction f
    let fType = createFuncT name args
    local (Map.insert name fType) (checkProgram (Program fs))

createFuncT :: Name -> [Arg] -> TType
createFuncT name args = 
    let toType :: Arg -> TType
        toType (Arg t id) = T t
    in TFunc (fmap toType args)

checkBlock :: Block -> RE ()
checkBlock (Block [] stmts) = do
    mapM_ checkStmt stmts
checkBlock (Block ((Decl t item):ds) stmts) = do
    case item of
        NoInit (Ident name) -> local (Map.insert name t') (checkBlock (Block ds stmts))
        Init (Ident name) expr -> do
            t' <- checkType expr t'          -- throws error if wrong type
            local (Map.insert name t') (checkBlock (Block ds stmts))
        where t' = T t
            -- case t of
            --     Bool -> TBool
            --     Str -> TStr
            --     Int -> TInt

-- declare :: Decl -> RE ()
-- declare (Decl t (i:is)) = do
--     case i of
--         NoInit (Ident name) -> local (Map.insert name t) declare (Decl t is)
--         Init (Ident name) -> local (Map.insert name t) declare (Decl t is)
-- declare (Decl t [])


checkStmt :: Stmt -> RE ()
checkStmt Empty = return ()
-- checkStmt (BStmt b) = checkBlock b
checkStmt (Ass lval expr) =
    case lval of 
        EVar (Ident name) -> do
            t <- findVar name       -- throws error if var not initialized
            -- t <- asks (Map.lookup name)
            t' <- checkType expr t-- throws error if t /= t'
            return ()
        -- EArrEl (Ident name) expr -> 
checkStmt (Incr (EVar (Ident name))) = do
    t <- findVar name
    compareTypes t (T Int)
    return ()
checkStmt (Decr (EVar (Ident name))) = do
    t <- findVar name
    compareTypes t (T Int)
    return ()
checkStmt (Ret expr) = do
    checkType expr (T Int)
    return ()
checkStmt (Cond expr b) = do
    checkType expr (T Bool)
    checkBlock b
checkStmt (CondElse expr b1 b2) = do
    checkType expr (T Bool)
    checkBlock b1
    checkBlock b2
checkStmt (While expr b) = do
    checkType expr (T Bool)
    checkBlock b
checkStmt (For ident expr b) = do           -- for i in range ...
    checkType expr (T Int)
    checkBlock b






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