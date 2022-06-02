-- File generated by the BNF Converter (bnfc 2.9.4).

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Pretty-printer for PrintMojeLatteBackup.

module PrintMojeLatteBackup where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified AbsMojeLatteBackup

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

instance Print AbsMojeLatteBackup.Ident where
  prt _ (AbsMojeLatteBackup.Ident i) = doc $ showString i
instance Print AbsMojeLatteBackup.Program where
  prt i = \case
    AbsMojeLatteBackup.Program topdefs -> prPrec i 0 (concatD [prt 0 topdefs])

instance Print AbsMojeLatteBackup.TopDef where
  prt i = \case
    AbsMojeLatteBackup.FnDef type_ id_ args block -> prPrec i 0 (concatD [prt 0 type_, prt 0 id_, doc (showString "("), prt 0 args, doc (showString ")"), prt 0 block])

instance Print [AbsMojeLatteBackup.TopDef] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print AbsMojeLatteBackup.Arg where
  prt i = \case
    AbsMojeLatteBackup.Arg type_ id_ -> prPrec i 0 (concatD [prt 0 type_, prt 0 id_])

instance Print [AbsMojeLatteBackup.Arg] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print AbsMojeLatteBackup.Block where
  prt i = \case
    AbsMojeLatteBackup.Block stmts -> prPrec i 0 (concatD [doc (showString "{"), prt 0 stmts, doc (showString "}")])

instance Print [AbsMojeLatteBackup.Stmt] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print AbsMojeLatteBackup.Stmt where
  prt i = \case
    AbsMojeLatteBackup.Empty -> prPrec i 0 (concatD [doc (showString ";")])
    AbsMojeLatteBackup.BStmt block -> prPrec i 0 (concatD [prt 0 block])
    AbsMojeLatteBackup.Decl type_ items -> prPrec i 0 (concatD [prt 0 type_, prt 0 items, doc (showString ";")])
    AbsMojeLatteBackup.Ass lvalue expr -> prPrec i 0 (concatD [prt 0 lvalue, doc (showString "="), prt 0 expr, doc (showString ";")])
    AbsMojeLatteBackup.Incr lvalue -> prPrec i 0 (concatD [prt 0 lvalue, doc (showString "++"), doc (showString ";")])
    AbsMojeLatteBackup.Decr lvalue -> prPrec i 0 (concatD [prt 0 lvalue, doc (showString "--"), doc (showString ";")])
    AbsMojeLatteBackup.Ret expr -> prPrec i 0 (concatD [doc (showString "return"), prt 0 expr, doc (showString ";")])
    AbsMojeLatteBackup.Cond expr block -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 block])
    AbsMojeLatteBackup.CondElse expr block1 block2 -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 block1, doc (showString "else"), prt 0 block2])
    AbsMojeLatteBackup.While expr block -> prPrec i 0 (concatD [doc (showString "while"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 block])
    AbsMojeLatteBackup.For id_ expr block -> prPrec i 0 (concatD [doc (showString "for"), prt 0 id_, doc (showString "in"), doc (showString "range"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 block])
    AbsMojeLatteBackup.SExp expr -> prPrec i 0 (concatD [prt 0 expr, doc (showString ";")])

instance Print AbsMojeLatteBackup.Item where
  prt i = \case
    AbsMojeLatteBackup.NoInit id_ -> prPrec i 0 (concatD [prt 0 id_])
    AbsMojeLatteBackup.Init id_ expr -> prPrec i 0 (concatD [prt 0 id_, doc (showString "="), prt 0 expr])

instance Print [AbsMojeLatteBackup.Item] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print AbsMojeLatteBackup.Type where
  prt i = \case
    AbsMojeLatteBackup.Int -> prPrec i 0 (concatD [doc (showString "int")])
    AbsMojeLatteBackup.Str -> prPrec i 0 (concatD [doc (showString "string")])
    AbsMojeLatteBackup.Bool -> prPrec i 0 (concatD [doc (showString "boolean")])
    AbsMojeLatteBackup.Arr type_ n -> prPrec i 0 (concatD [prt 0 type_, doc (showString "["), prt 0 n, doc (showString "]")])
    AbsMojeLatteBackup.Arr2 type_ -> prPrec i 0 (concatD [prt 0 type_, doc (showString "["), doc (showString "]")])

instance Print [AbsMojeLatteBackup.Type] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print AbsMojeLatteBackup.LValue where
  prt i = \case
    AbsMojeLatteBackup.EVar id_ -> prPrec i 0 (concatD [prt 0 id_])
    AbsMojeLatteBackup.EArrEl id_ expr -> prPrec i 0 (concatD [prt 0 id_, doc (showString "["), prt 0 expr, doc (showString "]")])

instance Print AbsMojeLatteBackup.Expr where
  prt i = \case
    AbsMojeLatteBackup.Elval lvalue -> prPrec i 6 (concatD [prt 0 lvalue])
    AbsMojeLatteBackup.ELitInt n -> prPrec i 6 (concatD [prt 0 n])
    AbsMojeLatteBackup.ELitTrue -> prPrec i 6 (concatD [doc (showString "true")])
    AbsMojeLatteBackup.ELitFalse -> prPrec i 6 (concatD [doc (showString "false")])
    AbsMojeLatteBackup.EApp id_ exprs -> prPrec i 6 (concatD [prt 0 id_, doc (showString "("), prt 0 exprs, doc (showString ")")])
    AbsMojeLatteBackup.EString str -> prPrec i 6 (concatD [printString str])
    AbsMojeLatteBackup.Neg expr -> prPrec i 5 (concatD [doc (showString "-"), prt 6 expr])
    AbsMojeLatteBackup.Not expr -> prPrec i 5 (concatD [doc (showString "!"), prt 6 expr])
    AbsMojeLatteBackup.EMul expr1 mulop expr2 -> prPrec i 4 (concatD [prt 4 expr1, prt 0 mulop, prt 5 expr2])
    AbsMojeLatteBackup.EAdd expr1 addop expr2 -> prPrec i 3 (concatD [prt 3 expr1, prt 0 addop, prt 4 expr2])
    AbsMojeLatteBackup.ERel expr1 relop expr2 -> prPrec i 2 (concatD [prt 2 expr1, prt 0 relop, prt 3 expr2])
    AbsMojeLatteBackup.EAnd expr1 expr2 -> prPrec i 1 (concatD [prt 2 expr1, doc (showString "&&"), prt 1 expr2])
    AbsMojeLatteBackup.EOr expr1 expr2 -> prPrec i 0 (concatD [prt 1 expr1, doc (showString "||"), prt 0 expr2])

instance Print [AbsMojeLatteBackup.Expr] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print AbsMojeLatteBackup.AddOp where
  prt i = \case
    AbsMojeLatteBackup.Plus -> prPrec i 0 (concatD [doc (showString "+")])
    AbsMojeLatteBackup.Minus -> prPrec i 0 (concatD [doc (showString "-")])

instance Print AbsMojeLatteBackup.MulOp where
  prt i = \case
    AbsMojeLatteBackup.Times -> prPrec i 0 (concatD [doc (showString "*")])
    AbsMojeLatteBackup.Div -> prPrec i 0 (concatD [doc (showString "/")])
    AbsMojeLatteBackup.Mod -> prPrec i 0 (concatD [doc (showString "%")])

instance Print AbsMojeLatteBackup.RelOp where
  prt i = \case
    AbsMojeLatteBackup.LTH -> prPrec i 0 (concatD [doc (showString "<")])
    AbsMojeLatteBackup.LE -> prPrec i 0 (concatD [doc (showString "<=")])
    AbsMojeLatteBackup.GTH -> prPrec i 0 (concatD [doc (showString ">")])
    AbsMojeLatteBackup.GE -> prPrec i 0 (concatD [doc (showString ">=")])
    AbsMojeLatteBackup.EQU -> prPrec i 0 (concatD [doc (showString "==")])
    AbsMojeLatteBackup.NE -> prPrec i 0 (concatD [doc (showString "!=")])
