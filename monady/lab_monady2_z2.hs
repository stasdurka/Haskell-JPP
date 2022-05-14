import Control.Monad.Reader
import Control.Monad.State
import Data.Map

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)
-- renumber :: Tree a -> Tree Int
-- renumber t = r 0 t where
--     r :: Int -> Tree a -> Tree Int
--     r h (Node x l r) = Node h (r (h+1) l) (r (h+1) r)
--     r _ Empty = Empty

-- env = Int
-- m ~ Env -> Int == Int -> Int

-- renumberM :: MonadReader Int m => Tree a -> m (Tree Int)

renumberM :: Tree a -> Reader Int (Tree Int)
renumberM Empty = return Empty
renumberM (Node _ l r) = do
    l' <- local (+1) (renumberM l)
    r' <- local (+1) (renumberM r)
    n <- ask
    return (Node n l' r')

run :: Tree a -> Tree Int
run t = runReader (renumberM t) 0

-- b.
type Var = String
data Exp = EInt Int
     | EOp  Op Exp Exp
     | EVar Var
     | ELet Var Exp Exp  -- let var = e1 in e2

data Op = OpAdd | OpMul | OpSub

-- evalExp :: Exp -> Int
type Env = Map Var Int

evalM :: Exp -> Reader Env Int
evalM (EOp o e1 e2) = 
    let
        op :: Num a => Op -> a -> a -> a
        op e = case e of 
            OpAdd -> (+)
            OpMul -> (*)
            OpSub -> (-) 
    in do
        n1 <- evalM e1
        n2 <- evalM e2
        return $ (op o) n1 n2
evalM (EInt n) = return n
evalM (EVar v) = 
    -- do
    --     env <- ask
    --     return (env ! v)
    asks (! v)
     
evalM (ELet v e1 e2) = do
    n1 <- evalM e1
    n2 <- local (insert v n1) (evalM e2)
    return n2

runEval :: Exp -> Int
runEval t = runReader (evalM t) (fromList [])

test = ELet "x" (ELet "y" (EOp OpAdd (EInt 6) (EInt 9))
                      (EOp OpSub y (EInt 1)))
                (EOp OpMul x (EInt 3))
    where x = EVar "x"
          y = EVar "y"
