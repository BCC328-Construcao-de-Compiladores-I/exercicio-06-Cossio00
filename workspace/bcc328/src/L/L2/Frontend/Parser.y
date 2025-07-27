{
module L.L2.Frontend.LALRParser (l2Parser) where

import L.L2.Frontend.Lexer (Token(..), TokenType(..))
import L.L2.Frontend.Syntax
import Utils.Var
import Utils.Value
}

%name l2Parser
%tokentype { Token }
%error { parseError }

%token
  NUMBER        { Token _ (TNumber $$) }
  STRING        { Token _ (TString $$) }
  IDENT         { Token _ (TIdent $$) }
  '('           { Token _ TLParen }
  ')'           { Token _ TRParen }
  '+'           { Token _ TPlus }
  '-'           { Token _ TMinus }
  '*'           { Token _ TTimes }
  '/'           { Token _ TDiv }
  'read'        { Token _ TRead }
  'print'       { Token _ TPrint }
  ':='          { Token _ TAssign }
  ';'           { Token _ TSemi }
  ','           { Token _ TComma }
  'def'         { Token _ TDef }
  'in'          { Token _ TIn }
  'end'         { Token _ TEnd }
  EOF           { Token _ TEOF }

%right ':='
%left '+' '-'
%left '*' '/'

%%
L2
  : Stmts EOF           { L2 (reverse $1) }

Stmts
  : Stmts Stmt ';'      { $2 : $1 }
  | Stmts Stmt          { $2 : $1 }
  |                     { [] }

Stmt
  : IDENT ':=' Expr     { LAssign $1 $3 }
  | 'read' '(' STRING ',' IDENT ')' { LRead $3 $5 }
  | 'print' '(' Expr ')' { LPrint $3 }
  | 'def' IDENT ':=' Expr 'in' Stmts 'end' { Def $2 $4 (reverse $6) }

Expr
  : NUMBER              { LVal (VInt $1) }
  | STRING              { LVal (VStr $1) }
  | IDENT               { LVar $1 }
  | Expr '+' Expr       { LAdd $1 $3 }
  | Expr '-' Expr       { LMinus $1 $3 }
  | Expr '*' Expr       { LMul $1 $3 }
  | Expr '/' Expr       { LDiv $1 $3 }
  | '(' Expr ')'        { $2 }

{
parseError :: [Token] -> a
parseError tokens = error $ "Erro de parsing na entrada: " ++ show (take 5 tokens)
}