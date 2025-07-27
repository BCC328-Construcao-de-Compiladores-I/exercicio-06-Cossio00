{
module L.L1.Frontend.LALRParser (l1Parser) where
import L.L1.Frontend.Lexer (Token(..), Lexeme(..))
import L.L1.Frontend.Syntax (L1(..), S1(..), E1(..))
import Utils.Value (Value(..))
import Utils.Var (Var(..))
}

%name l1Parser L1
%tokentype { Token }
%error { parseError }

%token
  NUMBER    { Token _ (TNumber $$) }
  STRING    { Token _ (TString $$) }
  IDENT     { Token _ (TIdent $$) }
  '('       { Token _ TLParen }
  ')'       { Token _ TRParen }
  '+'       { Token _ TPlus }
  '-'       { Token _ TMinus }
  '*'       { Token _ TTimes }
  '/'       { Token _ TDiv }
  'read'    { Token _ TRead }
  'print'   { Token _ TPrint }
  ':='      { Token _ TAssign }
  ';'       { Token _ TSemi }
  ','       { Token _ TComma }

%right ':='
%left '+' '-'
%left '*' '/'

%%

L1 :: { L1 }
L1 : Stmts { L1 $1 }

Stmts :: { [S1] }
Stmts : Stmt ';' Stmts { $1 : $3 }
      | {- empty -}    { [] }

Stmt :: { S1 }
Stmt : IDENT ':=' Expr    { LAssign (Var $1) $3 }
     | 'read' '(' STRING ',' IDENT ')' { LRead $3 (Var $5) }
     | 'print' '(' Expr ')'       { LPrint $3 }

Expr :: { E1 }
Expr : NUMBER          { LVal (VInt $1) }
     | STRING         { LVal (VStr $1) }
     | IDENT          { LVar (Var $1) }
     | Expr '+' Expr  { LAdd $1 $3 }
     | Expr '-' Expr  { LMinus $1 $3 }
     | Expr '*' Expr  { LMul $1 $3 }
     | Expr '/' Expr  { LDiv $1 $3 }
     | '(' Expr ')'   { $2 }

{
parseError :: [Token] -> a
parseError tokens = error $ "Erro de parsing: " ++ show tokens
}