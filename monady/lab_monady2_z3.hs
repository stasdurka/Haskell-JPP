
-- 3. Monada State
-- a. Napisz funkcję, która ponumeruje wezly drzewa tak, ze kazdy z nich bedzie mial inny numer
-- renumberTree :: Tree a -> Tree Int

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
            otherwise -> (+1)


renumberTree :: Tree a -> Tree Int
renumberTree t = evalState (renumberMS t) 0

t = Node 'a' (Node 'b' tl Empty) tl
tl = Node 'c' (Node 'd' Empty Empty) (Node 'z' Empty Empty)
-- renumberTree t

-- b. Rozszerzmy język z poprzedniego zadania o instrukcje języka Tiny (patrz przedmiot Semantyka i Weryfikacja Programów)
-- Stmt:   S ::= skip | x := e | S1;S2
--         | if b then S1 else S2 | while b do S
type Var = String
data Exp = EInt Int
     | EOp  Op Exp Exp
     | EVar Var
    --  | ELet Var Exp Exp  -- let var = e1 in e2

data Op = OpAdd | OpMul | OpSub

data BExp = BTrue
          | BFalse
          | BNot BExp
          | BOp BOp BExp BExp
          | BCmp Cmp Exp Exp
          deriving Show
data BOp = BAnd | BOr deriving Show
data Cmp = Eq | Lt | Leq | Gt | Geq | Neq deriving Show

evalBExp :: BExp -> Bool
evalBExpM :: BExp -> State Bool

data Stmt = SSkip -- skip
          | SAssign Var Exp -- x := e
          | SNext Stmt Stmt -- S1 ; S2
          | SIf BExp Stmt Stmt -- if b then S1 else S2
          | SWhile BExp Stmt -- while b do S
          deriving Show


