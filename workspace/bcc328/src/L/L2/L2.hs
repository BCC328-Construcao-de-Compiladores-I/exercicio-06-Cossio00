module Main where

     import L.L2.Interpreter.Interp (interp)
     import L.L2.Frontend.Lexer (Token(..), TokenType(..), AlexPosn(..), lexer)
     import L.L2.Frontend.LALRParser
     import L.L2.Frontend.Syntax
     import L.L2.Frontend.AST (toTree)
     import Utils.Pretty
     import Utils.Value
     import Utils.Var
     import Data.Tree (drawTree)
     import System.Environment
     import System.FilePath
     import Control.Exception (try, SomeException, evaluate)
     import Control.Monad (mapM_)

     main :: IO ()
     main = do
       args <- getArgs
       let opts = parseOptions args
       runWithOptions opts

     runWithOptions :: [Option] -> IO ()
     runWithOptions opts = case opts of
       [Lexer file] -> lexerOnly file
       [Parser file] -> parserOnly file
       [Interpret file] -> interpret file
       [VM file] -> v1Compiler file
       [C file] -> cCompiler file
       _ -> helpMessage

     lexerOnly :: FilePath -> IO ()
     lexerOnly file = do
       content <- readFile file
       let tokens = lexer content
       mapM_ printToken tokens
       where
         printToken :: Token -> IO ()
         printToken (Token (AlexPn _ line col) lexeme) = case lexeme of
           TNumber n -> putStrLn $ "Número " ++ show n ++ " Linha:" ++ show line ++ " Coluna:" ++ show col
           TLParen -> putStrLn $ "Parêntese ( Linha:" ++ show line ++ " Coluna:" ++ show col
           TRParen -> putStrLn $ "Parêntese ) Linha:" ++ show line ++ " Coluna:" ++ show col
           TPlus -> putStrLn $ "Operador + Linha:" ++ show line ++ " Coluna:" ++ show col
           TMinus -> putStrLn $ "Operador - Linha:" ++ show line ++ " Coluna:" ++ show col
           TTimes -> putStrLn $ "Operador * Linha:" ++ show line ++ " Coluna:" ++ show col
           TDiv -> putStrLn $ "Operador / Linha:" ++ show line ++ " Coluna:" ++ show col
           TRead -> putStrLn $ "Palavra reservada read Linha:" ++ show line ++ " Coluna:" ++ show col
           TPrint -> putStrLn $ "Palavra reservada print Linha:" ++ show line ++ " Coluna:" ++ show col
           TAssign -> putStrLn $ "Atribuição := Linha:" ++ show line ++ " Coluna:" ++ show col
           TSemi -> putStrLn $ "Ponto e vírgula ; Linha:" ++ show line ++ " Coluna:" ++ show col
           TComma -> putStrLn $ "Vírgula , Linha:" ++ show line ++ " Coluna:" ++ show col
           TDef -> putStrLn $ "Palavra reservada def Linha:" ++ show line ++ " Coluna:" ++ show col
           TIn -> putStrLn $ "Palavra reservada in Linha:" ++ show line ++ " Coluna:" ++ show col
           TEnd -> putStrLn $ "Palavra reservada end Linha:" ++ show line ++ " Coluna:" ++ show col
           TString s -> putStrLn $ "String \"" ++ s ++ "\" Linha:" ++ show line ++ " Coluna:" ++ show col
           TIdent s -> putStrLn $ "Identificador " ++ show s ++ " Linha:" ++ show line ++ " Coluna:" ++ show col
           TError s -> putStrLn $ "Erro léxico: " ++ s ++ " na linha " ++ show line ++ " coluna " ++ show col
           TEOF -> return ()

     parserOnly :: FilePath -> IO ()
     parserOnly file = do
       content <- readFile file
       let tokens = lexer content
       result <- try $ evaluate (l2Parser tokens) :: IO (Either SomeException L2)
       case result of
         Left err -> putStrLn $ "Erro de parsing: " ++ show err
         Right ast -> do
           putStrLn "Árvore de Sintaxe Abstrata (AST):"
           putStrLn $ drawTree (toTree ast)

     interpret :: FilePath -> IO ()
     interpret file = do
       content <- readFile file
       let tokens = lexer content
       result <- try $ evaluate (l2Parser tokens) :: IO (Either SomeException L2)
       case result of
         Left err -> putStrLn $ "Erro de parsing: " ++ show err
         Right ast -> do
           output <- interp ast
           mapM_ putStrLn output

     v1Compiler :: FilePath -> IO ()
     v1Compiler file = error "Not implemented!"

     cCompiler :: FilePath -> IO ()
     cCompiler file = error "Not implemented!"

     helpMessage :: IO ()
     helpMessage
       = putStrLn $ unlines
           [ "L2 language"
           , "Usage: l2 [--lexer-only | --parse-only | --interpret | --help]"
           , "--lexer-only: does the lexical analysis of the input program."
           , "--parse-only: does the syntax analysis of the input program."
           , "--interpret: does the syntax analysis and interpret the input program."
           , "--v1: does the syntax and semantic analysis and then generates V1 code."
           , "--c: does the syntax and semantic analysis, generates C code and uses GCC to generate an executable."
           , "--help: prints this help message."
           ]

     data Option
       = Help
       | Lexer FilePath
       | Parser FilePath
       | Interpret FilePath
       | VM FilePath
       | C FilePath
       deriving (Eq, Show)

     parseOptions :: [String] -> [Option]
     parseOptions args =
       case args of
         ("--lexer-only" : arg : _) -> [Lexer arg]
         ("--parse-only" : arg : _) -> [Parser arg]
         ("--interpret" : arg : _) -> [Interpret arg]
         ("--v1" : arg : _) -> [VM arg]
         ("--c" : arg : _) -> [C arg]
         _ -> [Help]