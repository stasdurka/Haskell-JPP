-- File generated by the BNF Converter (bnfc 2.9.4).

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Pretty-printer for PrintMojeLatte.

module PrintMojeLatte where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified AbsMojeLatte

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 False (map ($ "") $ d []) ""
  where
  rend
    :: Int        -- ^ Indentation level.
    -> Bool       -- ^ Pending indentation to be output before next character?
    -> [String]
    -> ShowS
  rend i p = \case
      "["      :ts -> char '[' . rend i False ts
      "("      :ts -> char '(' . rend i False ts
      "{"      :ts -> onNewLine i     p . showChar   '{'  . new (i+1) ts
      "}" : ";":ts -> onNewLine (i-1) p . showString "};" . new (i-1) ts
      "}"      :ts -> onNewLine (i-1) p . showChar   '}'  . new (i-1) ts
      [";"]        -> char ';'
      ";"      :ts -> char ';' . new i ts
      t  : ts@(s:_) | closingOrPunctuation s
                   -> pending . showString t . rend i False ts
      t        :ts -> pending . space t      . rend i False ts
      []           -> id
    where
    -- Output character after pending indentation.
    char :: Char -> ShowS
    char c = pending . showChar c

    -- Output pending indentation.
    pending :: ShowS
    pending = if p then indent i else id

  -- Indentation (spaces) for given indentation level.
  indent :: Int -> ShowS
  indent i = replicateS (2*i) (showChar ' ')

  -- Continue rendering in new line with new indentation.
  new :: Int -> [String] -> ShowS
  new j ts = showChar '\n' . rend j True ts

  -- Make sure we are on a fresh line.
  onNewLine :: Int -> Bool -> ShowS
  onNewLine i p = (if p then id else showChar '\n') . indent i

  -- Separate given string from following text by a space (if needed).
  space :: String -> ShowS
  space t s =
    case (all isSpace t', null spc, null rest) of
      (True , _   , True ) -> []              -- remove trailing space
      (False, _   , True ) -> t'              -- remove trailing space
      (False, True, False) -> t' ++ ' ' : s   -- add space if none
      _                    -> t' ++ s
    where
      t'          = showString t []
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt i = concatD . map (prt i)

instance Print Char where
  prt _ c = doc (showChar '\'' . mkEsc '\'' c . showChar '\'')

instance Print String where
  prt _ = printString

printString :: String -> Doc
printString s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q = \case
  s | s == q -> showChar '\\' . showChar s
  '\\' -> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  s -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print AbsMojeLatte.Ident where
  prt _ (AbsMojeLatte.Ident i) = doc $ showString i
instance Print AbsMojeLatte.Program where
  prt i = \case
    AbsMojeLatte.Program topdefs -> prPrec i 0 (concatD [prt 0 topdefs])

instance Print AbsMojeLatte.TopDef where
  prt i = \case
    AbsMojeLatte.FnDef type_ id_ args block -> prPrec i 0 (concatD [prt 0 type_, prt 0 id_, doc (showString "("), prt 0 args, doc (showString ")"), prt 0 block])

instance Print [AbsMojeLatte.TopDef] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print AbsMojeLatte.Arg where
  prt i = \case
    AbsMojeLatte.Arg type_ id_ -> prPrec i 0 (concatD [prt 0 type_, prt 0 id_])

instance Print [AbsMojeLatte.Arg] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print AbsMojeLatte.Block where
  prt i = \case
    AbsMojeLatte.Block decls stmts -> prPrec i 0 (concatD [doc (showString "{"), doc (showString "let"), prt 0 decls, doc (showString "in"), prt 0 stmts, doc (showString "}")])

instance Print AbsMojeLatte.Decl where
  prt i = \case
    AbsMojeLatte.Decl type_ items -> prPrec i 0 (concatD [prt 0 type_, prt 0 items, doc (showString ";")])

instance Print AbsMojeLatte.Item where
  prt i = \case
    AbsMojeLatte.NoInit id_ -> prPrec i 0 (concatD [prt 0 id_])
    AbsMojeLatte.Init id_ expr -> prPrec i 0 (concatD [prt 0 id_, doc (showString "="), prt 0 expr])

instance Print [AbsMojeLatte.Item] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [AbsMojeLatte.Decl] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print [AbsMojeLatte.Stmt] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print AbsMojeLatte.Stmt where
  prt i = \case
    AbsMojeLatte.Empty -> prPrec i 0 (concatD [doc (showString ";")])
    AbsMojeLatte.BStmt block -> prPrec i 0 (concatD [prt 0 block])
    AbsMojeLatte.Ass lvalue expr -> prPrec i 0 (concatD [prt 0 lvalue, doc (showString "="), prt 0 expr, doc (showString ";")])
    AbsMojeLatte.Incr lvalue -> prPrec i 0 (concatD [prt 0 lvalue, doc (showString "++"), doc (showString ";")])
    AbsMojeLatte.Decr lvalue -> prPrec i 0 (concatD [prt 0 lvalue, doc (showString "--"), doc (showString ";")])
    AbsMojeLatte.Ret expr -> prPrec i 0 (concatD [doc (showString "return"), prt 0 expr, doc (showString ";")])
    AbsMojeLatte.Cond expr block -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 block])
    AbsMojeLatte.CondElse expr block1 block2 -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 block1, doc (showString "else"), prt 0 block2])
    AbsMojeLatte.While expr block -> prPrec i 0 (concatD [doc (showString "while"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 block])
    AbsMojeLatte.For id_ expr block -> prPrec i 0 (concatD [doc (showString "for"), prt 0 id_, doc (showString "in"), doc (showString "range"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 block])
    AbsMojeLatte.SExp expr -> prPrec i 0 (concatD [prt 0 expr, doc (showString ";")])

instance Print AbsMojeLatte.Type where
  prt i = \case
    AbsMojeLatte.Int -> prPrec i 0 (concatD [doc (showString "int")])
    AbsMojeLatte.Str -> prPrec i 0 (concatD [doc (showString "string")])
    AbsMojeLatte.Bool -> prPrec i 0 (concatD [doc (showString "boolean")])
    AbsMojeLatte.Arr type_ n -> prPrec i 0 (concatD [prt 0 type_, doc (showString "["), prt 0 n, doc (showString "]")])
    AbsMojeLatte.Arr2 type_ -> prPrec i 0 (concatD [prt 0 type_, doc (showString "["), doc (showString "]")])

instance Print [AbsMojeLatte.Type] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print AbsMojeLatte.LValue where
  prt i = \case
    AbsMojeLatte.EVar id_ -> prPrec i 0 (concatD [prt 0 id_])
    AbsMojeLatte.EArrEl id_ expr -> prPrec i 0 (concatD [prt 0 id_, doc (showString "["), prt 0 expr, doc (showString "]")])

instance Print AbsMojeLatte.Expr where
  prt i = \case
    AbsMojeLatte.Elval lvalue -> prPrec i 6 (concatD [prt 0 lvalue])
    AbsMojeLatte.ELitInt n -> prPrec i 6 (concatD [prt 0 n])
    AbsMojeLatte.ELitTrue -> prPrec i 6 (concatD [doc (showString "true")])
    AbsMojeLatte.ELitFalse -> prPrec i 6 (concatD [doc (showString "false")])
    AbsMojeLatte.EApp id_ exprs -> prPrec i 6 (concatD [prt 0 id_, doc (showString "("), prt 0 exprs, doc (showString ")")])
    AbsMojeLatte.EString str -> prPrec i 6 (concatD [printString str])
    AbsMojeLatte.Neg expr -> prPrec i 5 (concatD [doc (showString "-"), prt 6 expr])
    AbsMojeLatte.Not expr -> prPrec i 5 (concatD [doc (showString "!"), prt 6 expr])
    AbsMojeLatte.EMul expr1 mulop expr2 -> prPrec i 4 (concatD [prt 4 expr1, prt 0 mulop, prt 5 expr2])
    AbsMojeLatte.EAdd expr1 addop expr2 -> prPrec i 3 (concatD [prt 3 expr1, prt 0 addop, prt 4 expr2])
    AbsMojeLatte.ERel expr1 relop expr2 -> prPrec i 2 (concatD [prt 2 expr1, prt 0 relop, prt 3 expr2])
    AbsMojeLatte.EAnd expr1 expr2 -> prPrec i 1 (concatD [prt 2 expr1, doc (showString "&&"), prt 1 expr2])
    AbsMojeLatte.EOr expr1 expr2 -> prPrec i 0 (concatD [prt 1 expr1, doc (showString "||"), prt 0 expr2])

instance Print [AbsMojeLatte.Expr] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print AbsMojeLatte.AddOp where
  prt i = \case
    AbsMojeLatte.Plus -> prPrec i 0 (concatD [doc (showString "+")])
    AbsMojeLatte.Minus -> prPrec i 0 (concatD [doc (showString "-")])

instance Print AbsMojeLatte.MulOp where
  prt i = \case
    AbsMojeLatte.Times -> prPrec i 0 (concatD [doc (showString "*")])
    AbsMojeLatte.Div -> prPrec i 0 (concatD [doc (showString "/")])
    AbsMojeLatte.Mod -> prPrec i 0 (concatD [doc (showString "%")])

instance Print AbsMojeLatte.RelOp where
  prt i = \case
    AbsMojeLatte.LTH -> prPrec i 0 (concatD [doc (showString "<")])
    AbsMojeLatte.LE -> prPrec i 0 (concatD [doc (showString "<=")])
    AbsMojeLatte.GTH -> prPrec i 0 (concatD [doc (showString ">")])
    AbsMojeLatte.GE -> prPrec i 0 (concatD [doc (showString ">=")])
    AbsMojeLatte.EQU -> prPrec i 0 (concatD [doc (showString "==")])
    AbsMojeLatte.NE -> prPrec i 0 (concatD [doc (showString "!=")])