--  Vinicius Cossio de Oliveira 19.1.4004
module L.L1.Frontend.RecursiveParser where

import Control.Applicative
import Control.Monad (void)
import L.L1.Frontend.Lexer (Token(..), Lexeme(..), lexer)
import L.L1.Frontend.Syntax
import Control.Monad.Combinators.Expr
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import Utils.Pretty
import Utils.Value
import Utils.Var

-- tipo para parser
type Parser = MP.Parsec Void [Token]

-- tipo for erro de parser
type ParserError = MP.ParseErrorBundle [Token] Void

-- Parse de um programa l1
parseL1 :: Parser L1
parseL1 = L1 <$> MP.sepEndBy parseS1 (symbol TSemi)

-- Parse de um comando (S1)
parseS1 :: Parser S1
parseS1 = MP.choice
  [ parseAssign
  , parseRead
  , parsePrint
  ]

-- Parse de atribuição
parseAssign :: Parser S1
parseAssign = do
  var <- matchToken (\(Token _ l) -> case l of TIdent s -> Just s; _ -> Nothing) "identifier"
  symbol TAssign
  expr <- parseE1
  return $ LAssign (Var var) expr

-- Parse read
parseRead :: Parser S1
parseRead = do
  symbol TRead
  symbol TLParen
  str <- matchToken (\(Token _ l) -> case l of TString s -> Just s; _ -> Nothing) "string"
  symbol TComma
  var <- matchToken (\(Token _ l) -> case l of TIdent s -> Just s; _ -> Nothing) "identifier"
  symbol TRParen
  return $ LRead str (Var var)

-- Parse print
parsePrint :: Parser S1
parsePrint = do
  symbol TPrint
  symbol TLParen
  expr <- parseE1
  symbol TRParen
  return $ LPrint expr

-- Parse de expressão
parseE1 :: Parser E1
parseE1 = makeExprParser parseTerm operatorTable

-- Parse expressões terminais: values or variables
parseTerm :: Parser E1
parseTerm = MP.choice
  [ LVal . VInt <$> matchToken (\(Token _ l) -> case l of TNumber n -> Just n; _ -> Nothing) "number"
  , LVal . VStr <$> matchToken (\(Token _ l) -> case l of TString s -> Just s; _ -> Nothing) "string"
  , LVar . Var <$> matchToken (\(Token _ l) -> case l of TIdent s -> Just s; _ -> Nothing) "identifier"
  , MP.between (symbol TLParen) (symbol TRParen) parseE1
  ]

-- Tabela de operações para precedência de expressão
operatorTable :: [[Operator Parser E1]]
operatorTable =
  [ [ InfixL (LAdd <$ symbol TPlus)
    , InfixL (LMinus <$ symbol TMinus) ]
  , [ InfixL (LMul <$ symbol TTimes)
    , InfixL (LDiv <$ symbol TDiv) ]
  ]

-- combinar um lexema específico
symbol :: Lexeme -> Parser ()
symbol lx = void (MP.satisfy (\(Token _ l) -> l == lx)) MP.<?> show lx

-- Extrair um valor de um lexema específico
matchToken :: (Token -> Maybe a) -> String -> Parser a
matchToken f expected = do
  t@(Token _ l) <- MP.satisfy (\(Token _ l') -> case l' of
    TIdent _ -> case f (Token undefined l') of Just _ -> True; _ -> False
    TString _ -> case f (Token undefined l') of Just _ -> True; _ -> False
    TNumber _ -> case f (Token undefined l') of Just _ -> True; _ -> False
    TError _ -> True
    _ -> False) MP.<?> expected
  case f t of
    Just x -> return x
    Nothing -> fail $ "Expected " ++ expected ++ ", got " ++ show t

-- função de análise principal
l1Parser :: String -> Either String L1
l1Parser input = case MP.parse (parseL1 <* MP.eof) "" (lexer input) of
  Left err -> Left (show err)
  Right ast -> Right ast