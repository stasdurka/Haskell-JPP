{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant ==" #-}
{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Use when" #-}
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity
import Data.Maybe(fromMaybe)
-- import Data.Either(fromRight)
import Control.Monad (foldM)
import AbsMojeLatte


--
-- A syntax tree type for simple math, with variables
--
-- data Exp = IntE Int
--          | OpE  Op Exp Exp
--          | VarE String
--          | LetE String Exp Exp 

-- data Decl = VarD String Exp -- var x=e

-- data Stmt = S               -- skip
--         | AS String Exp     -- x:= e
--         | SeqS Stmt Stmt    -- S1; S2
--         | IfS Exp Stmt Stmt -- if b then S1 else S2 
--         | WhileS Exp Stmt   -- while b do S
--         | Block [Decl] Stmt -- begin [D] S end


-- type Op = Int -> Int -> Int

type Loc = Int

type Env = M.Map String Loc

type Store = M.Map Loc Val

type RSE a = ReaderT Env (StateT Store (ExceptT String Identity)) a

data Val = IntVal Integer | BoolVal Bool | StrVal String

-- Env -> Store -> Either String (a, Store)
{-

ReaderT r m a  ===   r -> m a
StateT s m a   ===   s -> m (a,s)
ExceptT e m a  ===   (m (Either e a))

a => Either String a
a => Store -> Either String (a, Store)
a => Env -> Store -> Either String (a, Store)

----

type ERS a = ExceptT String (ReaderT Env (StateT Store Identity)) a

ExceptT e m a  ===   (m (Either e a))
ReaderT r m a  ===   r -> m a
StateT s m a   ===   s -> m (a,s)

a => Store -> (a, Store)
a => Env -> Store -> (a, Store)
a => Env -> Store -> (Either String a, Store)


-}
evalRelOp GTH e1 e2 = e1 > e2
evalRelOp GE e1 e2 = e1 >= e2
evalRelOp LTH e1 e2 = e1 < e2
evalRelOp EQU e1 e2 = e1 == e2
evalRelOp NE e1 e2 = e1 /= e2
evalRelOp LE e1 e2 = e1 <= e2

evalMulOp Div e1 e2 = div e1 e2
evalMulOp Times e1 e2 = e1 * e2
evalMulOp Mod e1 e2 = e1 `mod` e2

evalAddOp Minus e1 e2 = e1 - e2
evalAddOp Plus e1 e2 = e1 + e2

alloc :: Store -> Loc
alloc m = if (M.null m) then 0 
          else let (i, w) = M.findMax m in i+1  

alloc' :: RSE Loc
alloc' = do 
  m <- get 
  if (M.null m) then return 0 
  else let (i, w) = M.findMax m in return (i+1) 
--  do
--    l1 <- alloc'
--    l2 <- alloc'
--    -- tutaj l1 == l2 (bo alloc' nie zapamiętuje w Store że l1 jest zajęte... :)
--    -- ale ponizej po każdym alloc' jest modify


--
-- The interpreter
--

evalMaybe :: String -> Maybe a -> RSE a
evalMaybe s Nothing = throwError s
evalMaybe s (Just a) = return a


evalExp :: Expr -> RSE Val

evalExp (Elval (EVar (Ident name))) = do
    env <- ask
    state <- get
    l <- evalMaybe "undefined variable" $ M.lookup name env
    v <- evalMaybe "undefined location" $ M.lookup l state   -- returns value if found
    return v
evalExp (ELitInt n) = return $ IntVal n
evalExp ELitFalse = return $ BoolVal False
evalExp ELitTrue = return $ BoolVal True
-- evalExp EApp (Ident name) exp = do
--     env <- ask
--     state <- get

evalExp (EString str) = return $ StrVal str
evalExp (Neg e) = do
    IntVal val <- evalExp e
    return $ IntVal (-val)
evalExp (Not e) = do
    BoolVal v <- evalExp e
    return $ BoolVal $ not v
evalExp (EMul e1 op e2) = do
    (IntVal v1) <- evalExp e1
    (IntVal v2) <- evalExp e2
    return $ IntVal $ evalMulOp op v1 v2
evalExp (EAdd e1 op e2) = do
    IntVal v1 <- evalExp e1
    IntVal v2 <- evalExp e2
    return $ IntVal $ evalAddOp op v1 v2
evalExp (ERel e1 op e2) = do
    IntVal v1 <- evalExp e1
    IntVal v2 <- evalExp e2
    return $ BoolVal $ evalRelOp op v1 v2
evalExp (EAnd e1 e2) = do
    BoolVal v1 <- evalExp e1
    BoolVal v2 <- evalExp e2
    return $ BoolVal $ v1 && v2
evalExp (EOr e1 e2) = do
    BoolVal v1 <- evalExp e1
    BoolVal v2 <- evalExp e2
    return $ BoolVal $ v1 || v2

---
---Exec statement
---

interpret :: Stmt -> RSE ()  
interpret Empty = return ()
-- interpret BStmt block = 
interpret (Ass (EVar (Ident x)) e) = do
    env <- ask
    l <- evalMaybe "undefined variable" (M.lookup x env)
    val <- evalExp e 
    modify (M.insert l val)
-- interpret (SeqS s1 s2) = do {interpret s1;interpret s2}
interpret (Incr (EVar (Ident x))) = do
    env <- ask
    state <- get
    l <- evalMaybe "undefined variable" (M.lookup x env)
    IntVal val <- evalMaybe "variable not initialized" $ M.lookup l state   -- returns value if found
    modify $ M.insert l $ IntVal $ val+1
interpret (Decr (EVar (Ident x))) = do
    env <- ask
    state <- get
    l <- evalMaybe "undefined variable" (M.lookup x env)
    IntVal val <- evalMaybe "variable not initialized" $ M.lookup l state   -- returns value if found
    modify $ M.insert l $ IntVal $ val-1
-- interpret (Ret expr) = ...
interpret (Cond e b1) = do 
  BoolVal cond <- evalExp e
  if cond == True then interpret (BStmt b1) else return ()
interpret (CondElse e b1 b2) = do 
  BoolVal cond <- evalExp e
  if cond == True then interpret (BStmt b1) else interpret (BStmt b2)
    
interpret (While e b) = do 
  BoolVal cond <- evalExp e 
  if cond == True then interpret (BStmt b) 
  else do {interpret (BStmt b); interpret (While e b)} 

interpret (BStmt (Block [] s)) =  interpret s

interpret (BStmt (Block ((Decl t item):ds) s)) =
    case item of
        Init x expr -> do
            l <- alloc'
            val <- evalExp expr
            modify (M.insert l val)
            local (M.insert x l) (interpret (Block ds s))
        NoInit x -> do
            l <- alloc'
            local (M.insert x l) (interpret (Block ds s))

-- type RSE a = ReaderT Env (StateT Store (ExceptT String Identity)) a

-- Env -> Store -> Either String (a, Store)

-- execStmt :: Stmt -> IO ()
-- execStmt s =
--   print $
--       runExcept $ execStateT (runReaderT  (interpretCatch s) M.empty) M.empty


-- interpretCatch :: Stmt -> RSE ()
-- interpretCatch s = do
--   interpret s `catchError` (\e -> modify (M.insert 17 (length e)))

--
-- Run the interpreter
--

-- main = print $ evalState (runReaderT (eval testE) M.empty) M.empty 
