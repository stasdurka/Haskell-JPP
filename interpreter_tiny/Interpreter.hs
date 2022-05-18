{-# LANGUAGE OverloadedStrings #-}

module Interpreter where

import qualified Data.Map as M
import Control.Monad.Reader
import Data.Maybe(fromMaybe)

import AbsTinylet
import PrintTinylet

--
-- The interpreter
--

type Interpreter = Reader (M.Map Ident Int)
--type Interpreter a = Reader (M.Map Ident Int) a

type Op = Int -> Int -> Int

evalBinOp:: Op -> Exp -> Exp -> Interpreter Int
evalBinOp op e1 e2 = -- liftM2 op e1 e2
  do
   x1 <- evalExp e1
   x2 <- evalExp e2
   return (op x1 x2)

evalExp :: Exp -> Interpreter Int 

evalExp (EInt _ n) = return $ fromInteger n

evalExp (EAdd _ e1 e2) = evalBinOp (+) e1 e2
evalExp (ESub _ e1 e2) = evalBinOp (-) e1 e2
evalExp (EMul _ e1 e2) = liftM2 (*) (evalExp e1) (evalExp e2)
evalExp (EDiv p e1 e2) = evalBinOp mojDiv e1 e2
  where mojDiv x y =
          if y == 0 then
            error $ show (fromMaybe (0,0) p) ++ " dzielenie przez 0!"
          else
            x `div` y


evalExp (EVar p x)       = do
  maybea <- asks (M.lookup x)
  let v = fromMaybe
          (error $ "nieznana zmienna "++printTree x++" na pozycji "++show (fromMaybe (0,0) p))
          maybea
  return v 


evalExp (ELet _ x e1 e2) = do
    v   <- evalExp e1
    local (M.insert x v) (evalExp e2)

--
-- Run the interpreter
--

eval :: Exp -> Int
eval e = runReader (evalExp e) M.empty


n :: BNFC'Position
n = Nothing  -- BNFC'NoPosition -- nie ma position

{- A simple test expression:

let x =
   let y = 6 + 9
   in y - 1
in x * 3
 
==>  42
-}

test = ELet n "x" (ELet n "y" (EAdd n (EInt n 6) (EInt n 9))
                      (ESub n y (EInt n 1)))
                (EMul n x (EInt n 3))
    where x = EVar n "x"
          y = EVar n "y"


{- Another test expression:

let x=14 in
  ((let x = 3 in x) * x)
-}

test2 = ELet n "x" (EInt n 14)
        (EMul n (ELet n "x" (EInt n 3) x) x)
  where x = EVar n "x"  -- x=ok, y=error
  

{- Division by zero:
 
let x = 14 in
let y = x + 1 in 
100 / (x - y + 1)
-}

test0 = ELet n "x" (EInt n 14)
          (ELet n "y" (EAdd n x (EInt n 1)) 
             (EDiv n (EInt n 100) (EAdd n (ESub n x y) (EInt n 1))))
  where x = EVar n "x"
        y = EVar n "y"

