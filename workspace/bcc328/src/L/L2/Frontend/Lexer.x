{
module L.L2.Frontend.Lexer (Token(..), TokenType(..), AlexPosn(..), lexer) where

import Utils.Var (Var(..))
import Utils.Value (Value(..))
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$graphic = [$alpha $digit _]

tokens :-

  $white+                       ;
  "--".*                        ;
  $digit+                       { \p s -> Token p (TNumber (read s)) }
  \"[^\"]*\"                    { \p s -> Token p (TString (init (tail s))) }
  $alpha $graphic*              { \p s -> Token p (keyword s) }
  ":="                          { \p _ -> Token p TAssign }
  ";"                           { \p _ -> Token p TSemi }
  ","                           { \p _ -> Token p TComma }
  "("                           { \p _ -> Token p TLParen }
  ")"                           { \p _ -> Token p TRParen }
  "+"                           { \p _ -> Token p TPlus }
  "-"                           { \p _ -> Token p TMinus }
  "*"                           { \p _ -> Token p TTimes }
  "/"                           { \p _ -> Token p TDiv }
  .                             { \p s -> Token p (TError s) }

{
data Token = Token AlexPosn TokenType
  deriving (Eq, Show)

data TokenType
  = TNumber Int
  | TString String
  | TIdent Var
  | TLParen
  | TRParen
  | TPlus
  | TMinus
  | TTimes
  | TDiv
  | TRead
  | TPrint
  | TAssign
  | TSemi
  | TComma
  | TDef
  | TIn
  | TEnd
  | TError String
  | TEOF
  deriving (Eq, Show)

keyword :: String -> TokenType
keyword s = case s of
  "read"  -> TRead
  "print" -> TPrint
  "def"   -> TDef
  "in"    -> TIn
  "end"   -> TEnd
  _       -> TIdent (Var s)

lexer :: String -> [Token]
lexer str = go (alexStartPos, '\n', [], str)
  where
    go inp@(pos, _, _, str) =
      case alexScan inp 0 of
        AlexEOF -> [Token pos TEOF]
        AlexError (AlexPn _ line col, _, _, _) ->
          [Token (AlexPn 0 line col) (TError ("Erro léxico na linha " ++ show line ++ ", coluna " ++ show col))]
        AlexSkip inp' _ -> go inp'
        AlexToken inp' len act ->
          act pos (take len str) : go inp'
}