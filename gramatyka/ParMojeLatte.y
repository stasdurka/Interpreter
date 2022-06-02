-- -*- haskell -*- File generated by the BNF Converter (bnfc 2.9.4).

-- Parser definition for use with Happy
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module ParMojeLatte
  ( happyError
  , myLexer
  , pProgram
  ) where

import Prelude

import qualified AbsMojeLatte
import LexMojeLatte

}

%name pProgram Program
-- no lexer declaration
%monad { Err } { (>>=) } { return }
%tokentype {Token}
%token
  '!'       { PT _ (TS _ 1)  }
  '!='      { PT _ (TS _ 2)  }
  '%'       { PT _ (TS _ 3)  }
  '&&'      { PT _ (TS _ 4)  }
  '('       { PT _ (TS _ 5)  }
  ')'       { PT _ (TS _ 6)  }
  '*'       { PT _ (TS _ 7)  }
  '+'       { PT _ (TS _ 8)  }
  '++'      { PT _ (TS _ 9)  }
  ','       { PT _ (TS _ 10) }
  '-'       { PT _ (TS _ 11) }
  '--'      { PT _ (TS _ 12) }
  '/'       { PT _ (TS _ 13) }
  ';'       { PT _ (TS _ 14) }
  '<'       { PT _ (TS _ 15) }
  '<='      { PT _ (TS _ 16) }
  '='       { PT _ (TS _ 17) }
  '=='      { PT _ (TS _ 18) }
  '>'       { PT _ (TS _ 19) }
  '>='      { PT _ (TS _ 20) }
  'boolean' { PT _ (TS _ 21) }
  'else'    { PT _ (TS _ 22) }
  'false'   { PT _ (TS _ 23) }
  'for'     { PT _ (TS _ 24) }
  'if'      { PT _ (TS _ 25) }
  'in'      { PT _ (TS _ 26) }
  'int'     { PT _ (TS _ 27) }
  'let'     { PT _ (TS _ 28) }
  'range'   { PT _ (TS _ 29) }
  'return'  { PT _ (TS _ 30) }
  'string'  { PT _ (TS _ 31) }
  'true'    { PT _ (TS _ 32) }
  'while'   { PT _ (TS _ 33) }
  '{'       { PT _ (TS _ 34) }
  '||'      { PT _ (TS _ 35) }
  '}'       { PT _ (TS _ 36) }
  L_Ident   { PT _ (TV $$)   }
  L_integ   { PT _ (TI $$)   }
  L_quoted  { PT _ (TL $$)   }

%%

Ident :: { AbsMojeLatte.Ident }
Ident  : L_Ident { AbsMojeLatte.Ident $1 }

Integer :: { Integer }
Integer  : L_integ  { (read $1) :: Integer }

String  :: { String }
String   : L_quoted { $1 }

Program :: { AbsMojeLatte.Program }
Program : ListTopDef { AbsMojeLatte.Program $1 }

TopDef :: { AbsMojeLatte.TopDef }
TopDef
  : Type Ident '(' ListArg ')' Block { AbsMojeLatte.FnDef $1 $2 $4 $6 }

ListTopDef :: { [AbsMojeLatte.TopDef] }
ListTopDef : TopDef { (:[]) $1 } | TopDef ListTopDef { (:) $1 $2 }

Arg :: { AbsMojeLatte.Arg }
Arg : Type Ident { AbsMojeLatte.Arg $1 $2 }

ListArg :: { [AbsMojeLatte.Arg] }
ListArg
  : {- empty -} { [] }
  | Arg { (:[]) $1 }
  | Arg ',' ListArg { (:) $1 $3 }

Block :: { AbsMojeLatte.Block }
Block
  : '{' 'let' ListDecl 'in' ListStmt '}' { AbsMojeLatte.Block $3 $5 }

Decl :: { AbsMojeLatte.Decl }
Decl : Type Item ';' { AbsMojeLatte.Decl $1 $2 }

Item :: { AbsMojeLatte.Item }
Item
  : Ident { AbsMojeLatte.NoInit $1 }
  | Ident '=' Expr { AbsMojeLatte.Init $1 $3 }

ListDecl :: { [AbsMojeLatte.Decl] }
ListDecl : Decl { (:[]) $1 } | Decl ListDecl { (:) $1 $2 }

ListStmt :: { [AbsMojeLatte.Stmt] }
ListStmt : {- empty -} { [] } | Stmt ListStmt { (:) $1 $2 }

Stmt :: { AbsMojeLatte.Stmt }
Stmt
  : ';' { AbsMojeLatte.Empty }
  | Block { AbsMojeLatte.BStmt $1 }
  | LValue '=' Expr ';' { AbsMojeLatte.Ass $1 $3 }
  | LValue '++' ';' { AbsMojeLatte.Incr $1 }
  | LValue '--' ';' { AbsMojeLatte.Decr $1 }
  | 'return' Expr ';' { AbsMojeLatte.Ret $2 }
  | 'if' '(' Expr ')' Block { AbsMojeLatte.Cond $3 $5 }
  | 'if' '(' Expr ')' Block 'else' Block { AbsMojeLatte.CondElse $3 $5 $7 }
  | 'while' '(' Expr ')' Block { AbsMojeLatte.While $3 $5 }
  | 'for' Ident 'in' 'range' '(' Expr ')' Block { AbsMojeLatte.For $2 $6 $8 }

Type :: { AbsMojeLatte.Type }
Type
  : 'int' { AbsMojeLatte.Int }
  | 'string' { AbsMojeLatte.Str }
  | 'boolean' { AbsMojeLatte.Bool }

ListType :: { [AbsMojeLatte.Type] }
ListType
  : {- empty -} { [] }
  | Type { (:[]) $1 }
  | Type ',' ListType { (:) $1 $3 }

LValue :: { AbsMojeLatte.LValue }
LValue : Ident { AbsMojeLatte.EVar $1 }

Expr6 :: { AbsMojeLatte.Expr }
Expr6
  : LValue { AbsMojeLatte.Elval $1 }
  | Integer { AbsMojeLatte.ELitInt $1 }
  | 'true' { AbsMojeLatte.ELitTrue }
  | 'false' { AbsMojeLatte.ELitFalse }
  | Ident '(' ListExpr ')' { AbsMojeLatte.EApp $1 $3 }
  | String { AbsMojeLatte.EString $1 }
  | Expr7 { $1 }

Expr5 :: { AbsMojeLatte.Expr }
Expr5
  : '-' Expr6 { AbsMojeLatte.Neg $2 }
  | '!' Expr6 { AbsMojeLatte.Not $2 }
  | Expr6 { $1 }

Expr4 :: { AbsMojeLatte.Expr }
Expr4
  : Expr4 MulOp Expr5 { AbsMojeLatte.EMul $1 $2 $3 } | Expr5 { $1 }

Expr3 :: { AbsMojeLatte.Expr }
Expr3
  : Expr3 AddOp Expr4 { AbsMojeLatte.EAdd $1 $2 $3 } | Expr4 { $1 }

Expr2 :: { AbsMojeLatte.Expr }
Expr2
  : Expr2 RelOp Expr3 { AbsMojeLatte.ERel $1 $2 $3 } | Expr3 { $1 }

Expr1 :: { AbsMojeLatte.Expr }
Expr1 : Expr2 '&&' Expr1 { AbsMojeLatte.EAnd $1 $3 } | Expr2 { $1 }

Expr :: { AbsMojeLatte.Expr }
Expr : Expr1 '||' Expr { AbsMojeLatte.EOr $1 $3 } | Expr1 { $1 }

Expr7 :: { AbsMojeLatte.Expr }
Expr7 : '(' Expr ')' { $2 }

ListExpr :: { [AbsMojeLatte.Expr] }
ListExpr
  : {- empty -} { [] }
  | Expr { (:[]) $1 }
  | Expr ',' ListExpr { (:) $1 $3 }

AddOp :: { AbsMojeLatte.AddOp }
AddOp : '+' { AbsMojeLatte.Plus } | '-' { AbsMojeLatte.Minus }

MulOp :: { AbsMojeLatte.MulOp }
MulOp
  : '*' { AbsMojeLatte.Times }
  | '/' { AbsMojeLatte.Div }
  | '%' { AbsMojeLatte.Mod }

RelOp :: { AbsMojeLatte.RelOp }
RelOp
  : '<' { AbsMojeLatte.LTH }
  | '<=' { AbsMojeLatte.LE }
  | '>' { AbsMojeLatte.GTH }
  | '>=' { AbsMojeLatte.GE }
  | '==' { AbsMojeLatte.EQU }
  | '!=' { AbsMojeLatte.NE }

{

type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens

}

