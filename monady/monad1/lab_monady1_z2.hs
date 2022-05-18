import Data.Map as Map
-- c. zmień na Either zamiast Maybe,
-- d. rozszerz o data Exp = ... Var String
data Exp = Val Int | Div Exp Exp | Add Exp Exp | Var String

safediv :: Int -> Int -> Either String Int
safediv _ 0 = Left "Division by zero"
safediv x y = Right (div x y)

type Env = Map String Int

findVar :: Env -> String -> Either String Int
findVar e name = if member name e then Right (e ! name) else Left "Variable not initialized"

eval :: Env -> Exp -> Either String Int
-- ten sam kod co w either
eval _ (Val x) = return x     -- Right x
eval env (Div x y) = eval env x >>= \n -> 
                 eval env y >>= \m ->
                 safediv n m
eval env (Add x y) = eval env x >>= \n ->
                     eval env y >>= \m ->
                     return (n + m)
eval env (Var name) = findVar env name

evalList' :: Env -> [Exp] -> [Either String Int]
evalList' e l = fmap (eval e) l

evalList :: Env -> [Exp] -> Either String [Int]
evalList e l = mapM (eval e) l

env = fromList [("a", 0 :: Int), ("b", 2 :: Int)]
l = [Val 1, Var "a", Div (Val 3) (Var "b")] -- [1,0,1]
l2 = [Val 1, Var "a", Div (Val 3) (Var "a")] -- Division by 0
l3 = [Val 1, Var "z", Div (Val 3) (Var "a")] -- Variable not initialized


