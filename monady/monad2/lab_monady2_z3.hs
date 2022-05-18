-- 3. Monada State
-- a. Napisz funkcję, która ponumeruje wezly drzewa tak, ze kazdy z nich bedzie mial inny numer
-- renumberTree :: Tree a -> Tree Int
import Control.Monad.State
import Control.Monad.Reader
import Data.Functor.Classes (eq1)
import qualified Data.Map as M
import Data.List.NonEmpty (some1, xor)
import Data.Maybe(fromMaybe)


data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)

renumberMS :: Tree a -> State Int (Tree Int)
renumberMS Empty = return Empty
renumberMS (Node _ l r) = do
    s <- get
    modify (inc l)
    l' <- renumberMS l
    modify (inc r)
    r' <- renumberMS r
    return (Node s l' r')
    where
        inc :: Tree a -> Int -> Int
        inc t = case t of 
            Empty -> id
            _ -> (+1)

-- renumberMS :: Tree a -> State Int (Tree Int)
-- renumberMS Empty = return Empty
-- renumberMS (Node _ l r) = do
--     s <- get
--     do if (l == Empty) then 
--         do modify (+1)
--     l' <- renumberMS l
--     modify (+1)
--     r' <- renumberMS r
--     return (Node s l' r')
-- type Env = Map String Int


renumberTree :: Tree a -> Tree Int
renumberTree t = evalState (renumberMS t) 0

t = Node 'a' (Node 'b' tl Empty) tl
tl = Node 'c' (Node 'd' Empty Empty) (Node 'z' Empty Empty)
-- renumberTree t

-- b. 
-- Rozszerzmy język z poprzedniego zadania o instrukcje języka Tiny (patrz przedmiot Semantyka i Weryfikacja Programów)
-- Stmt:   S ::= skip | x := e | S1;S2
--         | if b then S1 else S2 | while b do S
-- type Var = String
-- data Exp = EInt Int
--      | EOp  Op Exp Exp
--      | EVar Var
--     --  | ELet Var Exp Exp  -- let var = e1 in e2

-- data Op = OpAdd | OpMul | OpSub



-- data Stmt = SSkip -- skip
--           | SAssign Var Exp -- x := e
--           | SNext Stmt Stmt -- S1 ; S2
--           | SIf BExp Stmt Stmt -- if b then S1 else S2
--           | SWhile BExp Stmt -- while b do S
--         --   deriving Show

-- -- evalStmtM :: MonadState State m => Stmt -> m State
-- evalStmtM :: Stmt -> State Env
-- evalStmtM SSkip         = get
-- evalStmtM (SAssign v e) = do
--     s <- get
--     let n = evalExpM e s

-- b.

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

type InterpMonad = State (M.Map String Int)

evalExp :: Exp -> InterpMonad Int
evalExp (IntE n) = return n
evalExp (VarE x) = do
    me <- gets $ M.lookup x
    return $ fromMaybe (error "Variable not declared") me
evalExp (OpE op e1 e2) = do
    n <- evalExp e1
    m <- evalExp e2
    return (op n m)


interpret :: Stmt -> InterpMonad ()
interpret S = return ()
interpret (AS x e) = do
    w <- evalExp e
    modify (\m -> M.insert x w m)

interpret (SeqS s1 s2) = do
    interpret s1
    interpret s2

interpret (IfS e s1 s2) = do
    w <- evalExp e
    if w==0 then interpret s2 
            else interpret s1

interpret (WhileS e s1) = do
    w <- evalExp e
    if w==0 then interpret S -- skip == return()
            else do
                interpret s1;
                interpret (WhileS e s1)

-- M.toList ::  Map k a -> [(k, a)]
execStmt :: Stmt -> IO ()
execStmt s = wypiszEnv $ execState (interpret s) M.empty
  where
    wypiszPare :: (String, Int) -> IO ()
    wypiszPare (s, i) = do {putStr $ s++", "; putStrLn $ show i}  
    wypiszEnv :: M.Map String Int -> IO ()
    wypiszEnv = mapM_ wypiszPare . M.toList

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

