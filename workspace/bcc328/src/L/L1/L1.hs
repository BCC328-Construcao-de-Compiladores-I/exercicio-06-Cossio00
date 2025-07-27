-- Vinicius Cossio de Oliveira 19.1.4004
module Main where

import qualified L.L1.Frontend.Lexer as Lexer
import qualified L.L1.Frontend.RecursiveParser as RecursiveParser
import qualified L.L1.Frontend.LALRParser as LALRParser
import L.L1.Backend.CCodegen
import L.L1.Backend.V1Codegen
import L.L1.Interpreter.Interp
import L.L1.Frontend.AST (toTree)
import L.L1.Frontend.Syntax
import Data.Tree (drawTree)
import Utils.Pretty
import Utils.Repl
import Utils.Value
import V.V1.Instr
import System.Environment
import System.FilePath
import System.Process
import Control.Exception (try, SomeException, evaluate)

main :: IO ()
main = do
  args <- getArgs
  runWithOptions (parseOptions args)

runWithOptions :: [Option] -> IO ()
runWithOptions opts = case opts of
  [Lexer file] -> alexBasedLexer file
  [Recursive file] -> recursiveParser file
  [LALR file] -> lalrParser file
  [Help] -> helpMessage
  _ -> helpMessage

alexBasedLexer :: FilePath -> IO ()
alexBasedLexer file = do
  content <- readFile file
  let tokens = Lexer.lexer content
  mapM_ printToken tokens
  where
    printToken :: Lexer.Token -> IO ()
    printToken (Lexer.Token (line, col) lexeme) = case lexeme of
      Lexer.TNumber n -> putStrLn $ "Número " ++ show n ++ " Linha:" ++ show line ++ " Coluna:" ++ show col
      Lexer.TLParen -> putStrLn $ "Parêntese ( Linha:" ++ show line ++ " Coluna:" ++ show col
      Lexer.TRParen -> putStrLn $ "Parêntese ) Linha:" ++ show line ++ " Coluna:" ++ show col
      Lexer.TPlus -> putStrLn $ "Operador + Linha:" ++ show line ++ " Coluna:" ++ show col
      Lexer.TMinus -> putStrLn $ "Operador - Linha:" ++ show line ++ " Coluna:" ++ show col
      Lexer.TTimes -> putStrLn $ "Operador * Linha:" ++ show line ++ " Coluna:" ++ show col
      Lexer.TDiv -> putStrLn $ "Operador / Linha:" ++ show line ++ " Coluna:" ++ show col
      Lexer.TRead -> putStrLn $ "Palavra reservada read Linha:" ++ show line ++ " Coluna:" ++ show col
      Lexer.TPrint -> putStrLn $ "Palavra reservada print Linha:" ++ show line ++ " Coluna:" ++ show col
      Lexer.TAssign -> putStrLn $ "Atribuição := Linha:" ++ show line ++ " Coluna:" ++ show col
      Lexer.TSemi -> putStrLn $ "Ponto e vírgula ; Linha:" ++ show line ++ " Coluna:" ++ show col
      Lexer.TComma -> putStrLn $ "Vírgula , Linha:" ++ show line ++ " Coluna:" ++ show col
      Lexer.TString s -> putStrLn $ "String \"" ++ s ++ "\" Linha:" ++ show line ++ " Coluna:" ++ show col
      Lexer.TIdent s -> putStrLn $ "Identificador " ++ s ++ " Linha:" ++ show line ++ " Coluna:" ++ show col
      Lexer.TError s -> putStrLn $ "Erro léxico: " ++ s ++ " na linha " ++ show line ++ " coluna " ++ show col
      Lexer.TEOF -> return ()

recursiveParser :: FilePath -> IO ()
recursiveParser file = do
  content <- readFile file
  case RecursiveParser.l1Parser content of
    Left err -> putStrLn $ "Erro de parsing: " ++ err
    Right ast -> do
      putStrLn "Árvore de Sintaxe Abstrata (AST):"
      putStrLn $ drawTree (toTree ast)

lalrParser :: FilePath -> IO ()
lalrParser file = do
  content <- readFile file
  let tokens = Lexer.lexer content
  result <- try $ evaluate (LALRParser.l1Parser tokens) :: IO (Either SomeException L1)
  case result of
    Left err -> putStrLn $ "Erro de parsing: " ++ show err
    Right ast -> do
      putStrLn "Árvore de Sintaxe Abstrata (AST):"
      putStrLn $ drawTree (toTree ast)

helpMessage :: IO ()
helpMessage
  = putStrLn $ unlines
      [ "L1 language"
      , "Usage: l1 [--lexer-only | --recursive | --lalr | --help]"
      , "--lexer-only: faz a análise léxica do programa de ebtrada usando um lexer baseado em Alex"
      , "--recursive: Faz análise sintática usando um analisador descendente recursivo utilizando Megaparsec e exibe o AST como uma árvore"
      , "--lalr: Faz análise sintática usando LALR e exibe o AST como uma árvore"
      , "--help: imprime mensagem de ajuda"
      ]

data Option
  = Help
  | Lexer FilePath
  | Recursive FilePath
  | LALR FilePath
  deriving (Eq, Show)

parseOptions :: [String] -> [Option]
parseOptions args =
  case args of
    ["--lexer-only", arg] -> [Lexer arg]
    ["--recursive", arg] -> [Recursive arg]
    ["--lalr", arg] -> [LALR arg]
    _ -> [Help]