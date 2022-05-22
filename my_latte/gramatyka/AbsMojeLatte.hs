-- File generated by the BNF Converter (bnfc 2.9.4).

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The abstract syntax of language mojeLatte.

module AbsMojeLatte where

import Prelude (Integer, String)
import qualified Prelude as C (Eq, Ord, Show, Read)
import qualified Data.String

data Program = Program [TopDef]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data TopDef = FnDef Type Ident [Arg] Block
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Arg = Arg Type Ident
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Block = Block [Decl] [Stmt]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Decl = Decl Type Item
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Item = NoInit Ident | Init Ident Expr
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Stmt
    = Empty
    | BStmt Block
    | Ass LValue Expr
    | Incr LValue
    | Decr LValue
    | Ret Expr
    | Cond Expr Block
    | CondElse Expr Block Block
    | While Expr Block
    | For Ident Expr Block
    | SExp Expr
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Type = Int | Str | Bool | Arr Type Integer | Arr2 Type
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data LValue = EVar Ident | EArrEl Ident Expr
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Expr
    = Elval LValue
    | ELitInt Integer
    | ELitTrue
    | ELitFalse
    | EApp Ident [Expr]
    | EString String
    | Neg Expr
    | Not Expr
    | EMul Expr MulOp Expr
    | EAdd Expr AddOp Expr
    | ERel Expr RelOp Expr
    | EAnd Expr Expr
    | EOr Expr Expr
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data AddOp = Plus | Minus
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data MulOp = Times | Div | Mod
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data RelOp = LTH | LE | GTH | GE | EQU | NE
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Ident = Ident String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)
