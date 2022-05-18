
import qualified Data.Map as M
import Control.Monad.State
import Data.Maybe(fromMaybe)

--
-- A syntax tree type for simple math, with variables
--
data Exp = IntE Int
         | OpE  Op Exp Exp
         | VarE String
--         | LetE String Exp Exp 

data Stmt = S               -- skip
        | AS String Exp     -- x:= e
        | SeqS Stmt Stmt    -- S1; S2
        | IfS Exp Stmt Stmt -- if b then S1 else S2 
        | WhileS Exp Stmt   -- while b do S


type Op = Int -> Int -> Int


--
-- The interpreter
--

{-
newtype State s a = State{runState :: s -> (a, s)}

instance Monad (State s) where
   return a = State $ \x -> (a,x)
   (State f) >>= g = State $ \x -> let (v,x') = f x in
                             runState (g v) x'

evalState :: State s a -> s -> a
execState :: State s a -> s -> s

get :: State s s
put :: s -> State s () 

gets :: (s -> a) -> State s a
modify :: (s -> s) -> State s ()


get odczytuje stan
get :: State s s
get = State $ \x -> (x,x)


put zapisuje nowy stan
put :: s -> State s () 
put x =  State $ \_ -> ((),x)


gets :: (s -> a) -> State s a
gets f = State $ \x -> (f x, x) 
   

modify :: (s -> s) -> State s ()
modify f = State $ \x -> ((), f x)


-}

---
---Exec statement
---

type InterpMonad = State (M.Map String Int)

interpret :: Stmt -> InterpMonad ()

interpret S = return ()

interpret (AS x e) = do
  w <- evalExpression e
  modify (\m -> M.insert x w m)
  
--insert :: Ord k => k -> a -> Map k a -> Map k a

interpret (SeqS s1 s2) = do
  interpret s1
  interpret s2

interpret (IfS e s1 s2) = do 
  w <- evalExpression e
  if w==0 then interpret s2 else interpret s1
    
interpret (WhileS e s1) = do 
  w <- evalExpression e
  if w==0 then interpret S else do {interpret s1; interpret (WhileS e s1)} 


evalExpression :: Exp -> InterpMonad Int

evalExpression (IntE n) = return n
evalExpression (VarE x) = do
  me <- gets $ M.lookup x
  return $ fromMaybe (error "Nie ma takiej zminnej") me
evalExpression (OpE op e1 e2) =
  liftM2 (op) (evalExpression e1) (evalExpression e2)

-- s -> (a, s)
  
-- M.toList ::  Map k a -> [(k, a)]
execStmt :: Stmt -> IO ()
execStmt s = wypiszEnv $ execState (interpret s) M.empty
  where
    wypiszPare :: (String, Int) -> IO ()
    wypiszPare (s, i) = do {putStr $ s++", "; putStrLn $ show i}  
    wypiszEnv :: M.Map String Int -> IO ()
    wypiszEnv = mapM_ wypiszPare . M.toList


--
-- Run the interpreter
--

main = execStmt testS

--
-- A simple text expression:
--
--    8 + 2
--
testE = OpE (+) (IntE 8) (IntE 2)

testS =
  SeqS (AS "x" testE) (                            -- x := testE
  SeqS (AS "y" $ IntE 0) (                         -- y := 0
  WhileS (VarE "x") (                              -- while (x) {
    SeqS (AS "x" (OpE (-) (VarE "x") (IntE 2))) (  --   x := x - 2
    AS "y" (OpE (+) (VarE "y") (IntE 1)))          --   y := y + 1
  )))                                              -- }

