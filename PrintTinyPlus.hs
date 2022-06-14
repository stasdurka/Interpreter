-- File generated by the BNF Converter (bnfc 2.9.4).

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Pretty-printer for PrintTinyPlus.

module PrintTinyPlus where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified AbsTinyPlus

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

instance Print AbsTinyPlus.Ident where
  prt _ (AbsTinyPlus.Ident i) = doc $ showString i
instance Print AbsTinyPlus.Program where
  prt i = \case
    AbsTinyPlus.Program topdefs block -> prPrec i 0 (concatD [prt 0 topdefs, doc (showString "main"), prt 0 block])

instance Print AbsTinyPlus.TopDef where
  prt i = \case
    AbsTinyPlus.FnDef type_ id_ args block -> prPrec i 0 (concatD [prt 0 type_, prt 0 id_, doc (showString "("), prt 0 args, doc (showString ")"), prt 0 block])

instance Print [AbsTinyPlus.TopDef] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print AbsTinyPlus.Arg where
  prt i = \case
    AbsTinyPlus.Arg type_ id_ -> prPrec i 0 (concatD [prt 0 type_, prt 0 id_])
    AbsTinyPlus.ArrRef type_ id_ -> prPrec i 0 (concatD [prt 0 type_, prt 0 id_, doc (showString "["), doc (showString "]")])
    AbsTinyPlus.VarRef type_ id_ -> prPrec i 0 (concatD [prt 0 type_, doc (showString "&"), prt 0 id_])

instance Print [AbsTinyPlus.Arg] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print AbsTinyPlus.Block where
  prt i = \case
    AbsTinyPlus.Block decls stmt -> prPrec i 0 (concatD [doc (showString "["), prt 0 decls, doc (showString "]"), doc (showString "{"), prt 0 stmt, doc (showString "}")])
    AbsTinyPlus.NoDecl stmt -> prPrec i 0 (concatD [doc (showString "{"), prt 0 stmt, doc (showString "}")])

instance Print AbsTinyPlus.Decl where
  prt i = \case
    AbsTinyPlus.Decl type_ item -> prPrec i 0 (concatD [prt 0 type_, prt 0 item])

instance Print AbsTinyPlus.Item where
  prt i = \case
    AbsTinyPlus.NoInit id_ -> prPrec i 0 (concatD [prt 0 id_])
    AbsTinyPlus.Init id_ expr -> prPrec i 0 (concatD [prt 0 id_, doc (showString "="), prt 0 expr])
    AbsTinyPlus.ArrInit id_ expr -> prPrec i 0 (concatD [prt 0 id_, doc (showString "["), prt 0 expr, doc (showString "]")])

instance Print [AbsTinyPlus.Decl] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print AbsTinyPlus.Stmt where
  prt i = \case
    AbsTinyPlus.Seq stmt1 stmt2 -> prPrec i 0 (concatD [prt 0 stmt1, prt 0 stmt2])
    AbsTinyPlus.Empty -> prPrec i 0 (concatD [doc (showString ";")])
    AbsTinyPlus.BStmt block -> prPrec i 0 (concatD [prt 0 block])
    AbsTinyPlus.Ass lvalue expr -> prPrec i 0 (concatD [prt 0 lvalue, doc (showString "="), prt 0 expr, doc (showString ";")])
    AbsTinyPlus.Incr lvalue -> prPrec i 0 (concatD [prt 0 lvalue, doc (showString "++"), doc (showString ";")])
    AbsTinyPlus.Decr lvalue -> prPrec i 0 (concatD [prt 0 lvalue, doc (showString "--"), doc (showString ";")])
    AbsTinyPlus.Ret expr -> prPrec i 0 (concatD [doc (showString "return"), prt 0 expr, doc (showString ";")])
    AbsTinyPlus.Cond expr block -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 block])
    AbsTinyPlus.CondElse expr block1 block2 -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 block1, doc (showString "else"), prt 0 block2])
    AbsTinyPlus.While expr block -> prPrec i 0 (concatD [doc (showString "while"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 block])
    AbsTinyPlus.For id_ expr block -> prPrec i 0 (concatD [doc (showString "for"), prt 0 id_, doc (showString "in"), doc (showString "range"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 block])
    AbsTinyPlus.Print expr -> prPrec i 0 (concatD [doc (showString "print"), prt 0 expr, doc (showString ";")])

instance Print AbsTinyPlus.Type where
  prt i = \case
    AbsTinyPlus.Int -> prPrec i 0 (concatD [doc (showString "int")])
    AbsTinyPlus.Str -> prPrec i 0 (concatD [doc (showString "string")])
    AbsTinyPlus.Bool -> prPrec i 0 (concatD [doc (showString "boolean")])

instance Print [AbsTinyPlus.Type] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print AbsTinyPlus.LValue where
  prt i = \case
    AbsTinyPlus.EVar id_ -> prPrec i 0 (concatD [prt 0 id_])
    AbsTinyPlus.EArrEl id_ expr -> prPrec i 0 (concatD [prt 0 id_, doc (showString "["), prt 0 expr, doc (showString "]")])

instance Print AbsTinyPlus.Expr where
  prt i = \case
    AbsTinyPlus.Elval lvalue -> prPrec i 6 (concatD [prt 0 lvalue])
    AbsTinyPlus.ELitInt n -> prPrec i 6 (concatD [prt 0 n])
    AbsTinyPlus.ELitTrue -> prPrec i 6 (concatD [doc (showString "true")])
    AbsTinyPlus.ELitFalse -> prPrec i 6 (concatD [doc (showString "false")])
    AbsTinyPlus.EApp id_ exprs -> prPrec i 6 (concatD [prt 0 id_, doc (showString "("), prt 0 exprs, doc (showString ")")])
    AbsTinyPlus.EString str -> prPrec i 6 (concatD [printString str])
    AbsTinyPlus.Neg expr -> prPrec i 5 (concatD [doc (showString "-"), prt 6 expr])
    AbsTinyPlus.Not expr -> prPrec i 5 (concatD [doc (showString "!"), prt 6 expr])
    AbsTinyPlus.EMul expr1 mulop expr2 -> prPrec i 4 (concatD [prt 4 expr1, prt 0 mulop, prt 5 expr2])
    AbsTinyPlus.EAdd expr1 addop expr2 -> prPrec i 3 (concatD [prt 3 expr1, prt 0 addop, prt 4 expr2])
    AbsTinyPlus.ERel expr1 relop expr2 -> prPrec i 2 (concatD [prt 2 expr1, prt 0 relop, prt 3 expr2])
    AbsTinyPlus.EAnd expr1 expr2 -> prPrec i 1 (concatD [prt 2 expr1, doc (showString "&&"), prt 1 expr2])
    AbsTinyPlus.EOr expr1 expr2 -> prPrec i 0 (concatD [prt 1 expr1, doc (showString "||"), prt 0 expr2])

instance Print [AbsTinyPlus.Expr] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print AbsTinyPlus.AddOp where
  prt i = \case
    AbsTinyPlus.Plus -> prPrec i 0 (concatD [doc (showString "+")])
    AbsTinyPlus.Minus -> prPrec i 0 (concatD [doc (showString "-")])

instance Print AbsTinyPlus.MulOp where
  prt i = \case
    AbsTinyPlus.Times -> prPrec i 0 (concatD [doc (showString "*")])
    AbsTinyPlus.Div -> prPrec i 0 (concatD [doc (showString "/")])
    AbsTinyPlus.Mod -> prPrec i 0 (concatD [doc (showString "%")])

instance Print AbsTinyPlus.RelOp where
  prt i = \case
    AbsTinyPlus.LTH -> prPrec i 0 (concatD [doc (showString "<")])
    AbsTinyPlus.LE -> prPrec i 0 (concatD [doc (showString "<=")])
    AbsTinyPlus.GTH -> prPrec i 0 (concatD [doc (showString ">")])
    AbsTinyPlus.GE -> prPrec i 0 (concatD [doc (showString ">=")])
    AbsTinyPlus.EQU -> prPrec i 0 (concatD [doc (showString "==")])
    AbsTinyPlus.NE -> prPrec i 0 (concatD [doc (showString "!=")])
