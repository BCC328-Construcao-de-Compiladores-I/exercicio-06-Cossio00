module L.L2.Backend.CCodegen (
  CCompiler,
  compile,
  compileStmt,
  compileExpr,
  escapeString,
  exprType,
  concatMapM,
  varName
) where

import L.L2.Frontend.LALRParser (l2Parser)
import L.L2.Frontend.Lexer (lexer)
import L.L2.Frontend.Syntax
import Utils.Value
import Utils.Var
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.State
import System.IO
import System.Process
import System.FilePath
import Control.Exception (try, SomeException, evaluate)
import Text.Printf

type CCompiler a = ExceptT String (StateT (Map Var Value) IO) a

varName :: Var -> String
varName (Var s) = s

compile :: L2 -> CCompiler String
compile (L2 stmts) = do
  let header = unlines [
        "#include <stdio.h>",
        "#include <stdlib.h>",
        "#include <string.h>",
        "",
        "char* concat(const char* s1, const char* s2) {",
        "  size_t len1 = strlen(s1);",
        "  size_t len2 = strlen(s2);",
        "  char* result = malloc(len1 + len2 + 1);",
        "  strcpy(result, s1);",
        "  strcat(result, s2);",
        "  return result;",
        "}",
        "",
        "char* itoa(int value, char* str, int base) {",
        "  snprintf(str, 32, \"%d\", value);",
        "  return str;",
        "}",
        "",
        "int main() {"
        ]
  body <- concatMapM compileStmt stmts
  let footer = "  return 0;\n}"
  return $ header ++ body ++ footer

compileStmt :: S2 -> CCompiler String
compileStmt (LAssign v e) = do
  env <- get
  expr <- compileExpr e
  case Map.lookup v env of
    Just _ -> return $ printf "  %s = %s;\n" (varName v) expr
    Nothing -> do
      put $ Map.insert v (VInt 0) env
      return $ printf "  int %s = %s;\n" (varName v) expr
compileStmt (LRead s v) = do
  env <- get
  case Map.lookup v env of
    Just _ -> return ()
    Nothing -> put $ Map.insert v (VInt 0) env
  let prompt = escapeString s
  return $ printf "  printf(\"%s\"); scanf(\"%%d\", &%s);\n" prompt (varName v)
compileStmt (LPrint e) = do
  expr <- compileExpr e
  return $ case exprType e of
    VInt _ -> printf "  printf(\"%%d\\n\", %s);\n" expr
    VStr _ -> printf "  printf(\"%%s\\n\", %s);\n" expr
compileStmt (Def v e stmts) = do
  env <- get
  put $ Map.insert v (VInt 0) env
  expr <- compileExpr e
  body <- concatMapM compileStmt stmts
  put env
  return $ printf "  {\n    int %s = %s;\n%s  }\n" (varName v) expr body

compileExpr :: E2 -> CCompiler String
compileExpr (LVal (VInt n)) = return $ show n
compileExpr (LVal (VStr s)) = return $ printf "\"%s\"" (escapeString s)
compileExpr (LVar v) = do
  env <- get
  case Map.lookup v env of
    Just _ -> return $ varName v
    Nothing -> throwError $ "Undefined variable: " ++ varName v
compileExpr (LAdd e1 e2) = do
  let t1 = exprType e1
  let t2 = exprType e2
  e1' <- compileExpr e1
  e2' <- compileExpr e2
  case (t1, t2) of
    (VInt _, VInt _) -> return $ printf "(%s + %s)" e1' e2'
    (VStr _, VInt _) -> return $ printf "concat(%s, itoa(%s, malloc(32), 10))" e1' e2'
    (VInt _, VStr _) -> return $ printf "concat(itoa(%s, malloc(32), 10), %s)" e1' e2'
    (VStr _, VStr _) -> return $ printf "concat(%s, %s)" e1' e2'
compileExpr _ = throwError "Unsupported expression"

escapeString :: String -> String
escapeString s = concatMap escapeChar s
  where
    escapeChar '\n' = "\\n"
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar c = [c]

exprType :: E2 -> Value
exprType (LVal v) = v
exprType (LVar _) = VInt 0
exprType (LAdd e1 e2) = case (exprType e1, exprType e2) of
  (VInt _, VInt _) -> VInt 0
  (VStr _, _) -> VStr ""
  (_, VStr _) -> VStr ""
exprType _ = VInt 0

concatMapM :: (a -> CCompiler String) -> [a] -> CCompiler String
concatMapM f xs = concat <$> mapM f xs