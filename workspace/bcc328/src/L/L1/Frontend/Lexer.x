{
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module L.L1.Frontend.Lexer (Token (..), Lexeme (..), lexer) where 
}
-- Vinicius Cossio 19.1.4004

%wrapper "posn"

$digit = [0-9]            -- digits

-- second RE macros

@number   = [\-]? $digit+
$alpha    = [a-zA-Z]
$alphanum = [$alpha $digit]


-- tokens declarations

tokens :-
      $white+       ;
      "//" .*       ;
      @number       {mkNumber}
      "("           { simpleToken TLParen }
      ")"           { simpleToken TRParen }
      "+"           { simpleToken TPlus }
      "-"           { simpleToken TMinus }
      "*"           { simpleToken TTimes }
      "/"           { simpleToken TDiv }
      "read"        { simpleToken TRead }
      "print"       { simpleToken TPrint }
      ":="          { simpleToken TAssign }
      ";"           { simpleToken TSemi }
      ","           { simpleToken TComma }
      \"[^\"]*\"                  { \p s -> Token (position p) (TString (init (tail s))) }
      $alpha [$alphanum]*         { \p s -> Token (position p) (TIdent s) }
      .                           { \p s -> Token (position p) (TError s) }

{
data Token
  = Token {
      pos :: (Int, Int)
    , lexeme :: Lexeme
    } deriving (Eq, Ord, Show)

data Lexeme
  = TNumber Int
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
  | TString String
  | TIdent String
  | TError String
  | TEOF
  deriving (Eq, Ord, Show)


position :: AlexPosn -> (Int, Int)
position (AlexPn _ x y) = (x, y)

mkNumber :: AlexPosn -> String -> Token
mkNumber p s = Token (position p) (TNumber $ read s)

simpleToken :: Lexeme -> AlexPosn -> String -> Token
simpleToken lx p _ = Token (position p) lx

lexer :: String -> [Token]
lexer = alexScanTokens
}
