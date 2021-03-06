import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity
import Data.Maybe(fromMaybe)
-- import Data.Either(fromRight)
import Control.Monad (foldM)


--
-- A syntax tree type for simple math, with variables
--
data Exp = IntE Int
         | OpE  Op Exp Exp
         | VarE String
         | LetE String Exp Exp 

data Decl = VarD String Exp -- var x=e

data Stmt = S               -- skip
        | AS String Exp     -- x:= e
        | SeqS Stmt Stmt    -- S1; S2
        | IfS Exp Stmt Stmt -- if b then S1 else S2 
        | WhileS Exp Stmt   -- while b do S
        | Block [Decl] Stmt -- begin [D] S end


type Op = Int -> Int -> Int

type Loc = Int

type Env = M.Map String Loc

type Store = M.Map Loc Int

type RSE a = ReaderT Env (StateT Store (ExceptT String Identity)) a

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

alloc :: Store-> Loc
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
  
  

eval :: Exp -> RSE Int

eval (IntE n)       = return n

eval (OpE op e1 e2) = do
   x1 <- eval e1
   x2 <- eval e2
   return (op x1 x2)


eval (VarE x)       = do
    env <- ask
    st  <- get
    l <- evalMaybe "undefined variable" $ M.lookup x env
    evalMaybe "undefined location" $ M.lookup l st


eval (LetE x e1 e2) = do
    v <- eval e1
    l <- alloc'
    modify (M.insert l v)       
    local (M.insert x l) (eval e2)

---
---Exec statement
---

interpret :: Stmt -> RSE ()  
interpret S = return () 

-- zmienna x juz byla zadeklarowana ! ktoś jej przypisał lokacje, updatuje stan
interpret (AS x e)  = 
 do
  env <- ask
  l <- evalMaybe "undefined variable" (M.lookup x env)
  w <- eval e 
  modify (M.insert l w)
   


interpret (SeqS s1 s2) = do {interpret s1;interpret s2}


interpret (IfS e s1 s2) = 
 do 
  w <- eval e 
  if w==0 then interpret s2 else interpret s1
    
interpret (WhileS e s1) = 
 do 
  w <- eval e 
  if w==0 then interpret S else do {interpret s1; interpret (WhileS e s1)} 

interpret (Block [] s) =  interpret s

interpret (Block ((VarD x e):ds) s) =  
 do
  l <- alloc'
  w <- eval e 
  modify (M.insert l w)
  local (M.insert x l) (interpret (Block ds s))

-- type RSE a = ReaderT Env (StateT Store (ExceptT String Identity)) a

-- Env -> Store -> Either String (a, Store)

execStmt :: Stmt -> IO ()
execStmt s =
  print $
      runExcept $ execStateT (runReaderT  (interpretCatch s) M.empty) M.empty


interpretCatch :: Stmt -> RSE ()
interpretCatch s = do
  interpret s `catchError` (\e -> modify (M.insert 17 (length e)))

--
-- Run the interpreter
--

-- main = print $ evalState (runReaderT (eval testE) M.empty) M.empty 


--
-- A simple text expression:
--
    --  let x =
    --      let y = 5 + 6
    --      in y / 5
    --  in x * 3
-- 
-- ==>  6
--
testE = LetE "x" (LetE "y" (OpE (+) (IntE 5) (IntE 6))
                      (OpE div y (IntE 5)))
                (OpE (*) x (IntE 3))
    where x = VarE "x"
          y = VarE "y"


-- x:=testE
-- while x { x:=x-1 }
testS = (SeqS (AS "x" testE) (WhileS (VarE "x") (AS "x" (OpE (-) (VarE "x") (IntE 1))))) 

testSB = Block [VarD "x" (IntE 3)] (SeqS (AS "x" testE) (WhileS (VarE "x") (AS "x" (OpE (-) (VarE "x") (IntE 1))))) 

testSB1 = Block [VarD "x" (IntE 3)] (AS "x" (IntE 4))

testSB2 = Block [VarD "x" (IntE 3), VarD "y" (IntE 7)] S

{- testPrgW =
   [x = 1000, y = 10] 
   while y {
     if x then
       {[x = -1] y:=y+x}   -- testIB1
     else
       {[x = 1]; y:=y-x};  -- testIB2
     x:=x+y
   };
-}

testIB1 = Block [VarD "x" (IntE (-1))] (AS "y" (OpE (+) (VarE "y") (VarE "x")))
testIB2 = Block [VarD "x" (IntE 1)] (AS "y" (OpE (-) (VarE "y") (VarE "x")))
testWB = SeqS (IfS (VarE "x") testIB1 testIB2) (AS "x" (OpE (+) (VarE "x") (VarE "y")))
testW = WhileS (VarE "y") testWB

testPrgW = Block [VarD "x" (IntE 1000), VarD "y" (IntE 10)] testW

{-
bad = {[y=42] y:=x}
-}
 
bad = Block [VarD "y" (IntE 42)] $ AS "y" (VarE "x")

-- execStmt bad
-- .... Left "undefined variable"

--
-- lift :: m a -> ReaderT r m a 
-- lift x = ReaderT $ \_ -> x

--
-- lift :: m a -> StateT s m a 
-- lift x = StateT $ \s -> do { a <- x; return (a,s) }
