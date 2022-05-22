module IntLambda where

infixr 5 :->
data Type = TInt | Type :-> Type 
  deriving Eq

type Name = String
data Exp = EInt Int | EVar Name 
         | ELam Name Type Exp | EApp Exp Exp

--          (\  (x  : t) -> e )

-- Przykładowe lambda-termy
type Exp1 = Type -> Exp
type Exp2 = Type -> Exp1  -- Type -> Type -> Exp
type Exp3 = Type -> Exp2  -- Type -> Type -> Type -> Exp

int :: Type
int = TInt

mkI :: Exp1
mkI a = ELam "x" a $ EVar "x"

mkK :: Exp2
mkK a b = ELam "x" a $ ELam "y" b $ EVar "x"

intK = mkK int int

mkS :: Exp3
mkS a b c = ELam "x" a $ ELam "y" b $ ELam "z" c
          $ EApp 
             (EApp (EVar "x") (EVar "z")) 
             (EApp (EVar "y") (EVar "z")) 

-- \x y z -> x z (y z)

intS = mkS (int:->int:->int) (int:->int) int

-- kombinator omega nie typuje sie w prostym rachunku lambda
mkOmega :: Exp1
mkOmega t = ELam "x" t $ EApp (EVar "x") (EVar "x")
-- \x -> x x

intOmega = mkOmega TInt

-- Show

instance Show Type where
  showsPrec d TInt = showString "int"
  showsPrec d (u :-> v) = showParen (d > arr_prec) $
             showsPrec (arr_prec+1) u . 
             showString " :-> "       . 
             showsPrec arr_prec v
          where arr_prec = 5

instance Show Exp where
  showsPrec d (EVar n) = showString n
  showsPrec d (EInt i) = showsPrec 10 i
  showsPrec d (EApp e1 e2) = showParen (d > ap_prec) $
             showsPrec (ap_prec) e1   .
             showString " "           .
             showsPrec (ap_prec+1) e2 
          where ap_prec = 10

  showsPrec d (ELam n t e) = showParen (d > lam_prec) $
             showString ("\\"++n++":"++show t++".") .
             showsPrec (lam_prec) e
          where lam_prec = 1
           
