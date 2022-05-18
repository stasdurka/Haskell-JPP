{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe(fromMaybe)

--
-- A syntax tree type for simple math, with variables
--
data Exp = IntE Int
         | OpE  Op Exp Exp
         | VarE String
--         | LetE String Exp Exp 

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

type RS a = ReaderT Env (State Store) a
--  Env -> Store -> (a, Store)

-- Będziemy mieli:
--   eval :: Exp -> RS Int
--   interpret :: Stmt -> RS ()

-- Wyposażenie monady

newloc :: Store -> Loc
newloc m = if M.null m then 0
          else let (i, w) = M.findMax m in i+1

newloc' :: RS Loc
newloc' = do
  m <- get
  return $ newloc m

-- UWAGA!
--  do
--    l1 <- newloc'
--    l2 <- newloc'
--    -- tutaj l1 == l2
--    -- (bo newloc' nie zapamiętuje w Store że l1 jest zajęte... :)
--    -- poniżej po l <- newloc' zaraz robimy modify (M.insert l ...)

--
-- The interpreter
--

eval :: Exp -> RS Int

eval (IntE n)       = return n

eval (OpE op e1 e2) = do
   x1 <- eval e1
   x2 <- eval e2
   return (op x1 x2)

eval (VarE x)       = do
    env <- ask
    st  <- get
    let l = fromMaybe (error "undefined variable") (M.lookup x env)
    return $ fromMaybe (error "undefined location") (M.lookup l st)

{-
eval (LetE x e1 e2) = do
    v <- eval e1
    l <- newloc'
    modify (M.insert l v)
    local (M.insert x l) (eval e2)
-}

---
---Exec statement
---

interpret :: Stmt -> RS ()
interpret S = return ()

-- zmienna x juz byla zadeklarowana ! ktoś jej przypisał lokację, updatujemy stan
interpret (AS x e)  =
 do
  env <- ask
  let l = fromMaybe (error "undefined variable") (M.lookup x env)
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
  l <- newloc'
  w <- eval e
  modify (M.insert l w)
  local (M.insert x l) (interpret (Block ds s))

--
-- Run the interpreter
--

exec :: Stmt -> Store
exec p =
  execState (runReaderT (interpret p) env0) store0
  where
    env0 = M.empty   -- albo np. M.fromList [("x",1),("y",2)]
    store0 = M.empty -- albo np. M.fromList [(1,42),(2,17)]

main = print $ exec testPrgW



{- testPrgW =
   [ x = 0; y = 10]
   while y {
     if x then
       {[x = -1] y:=y+x}
     else
       {[x = 1]; y:=y-x};
     x:=x+y
   };
-}

testIB1 = Block [VarD "x" (IntE (-1))] (AS "y" (OpE (+) (VarE "y") (VarE "x")))
testIB2 = Block [VarD "x" (IntE 1)] (AS "y" (OpE (-) (VarE "y") (VarE "x")))
testWB = SeqS (IfS (VarE "x") testIB1 testIB2) (AS "x" (OpE (+) (VarE "x") (VarE "y")))
testW = WhileS (VarE "y") testWB

testPrgW = Block [VarD "x" (IntE (0)), VarD "y" (IntE 10)] testW










--
-- lift :: m a -> ReaderT r m a 
-- lift ma = ReaderT $ \_ -> ma

--
-- lift :: m a -> StateT s m a 
-- lift ma = StateT $ \s -> do { a <- ma; return (a,s) }
--                          obliczenie w monadzie "m"

