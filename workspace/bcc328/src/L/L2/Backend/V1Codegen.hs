module L.L2.Backend.V1Codegen where

import L.L2.Frontend.Syntax
import V.V1.Instr
import Utils.Value
import Utils.Var
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.State
import Text.Printf

-- Compiler state for variable environment
type V1Compiler a = ExceptT String (State (Map Var Value)) a

-- Generate V1 code for an L2 program
v1Codegen :: L2 -> Code
v1Codegen (L2 stmts) = case runState (runExceptT (concatMapM compileStmt stmts)) Map.empty of
  (Right code, _) -> code
  (Left err, _) -> error $ "V1 codegen error: " ++ err

-- Compile a single statement
compileStmt :: S2 -> V1Compiler Code
compileStmt (LAssign v e) = do
  env <- get
  put $ Map.insert v (exprType e) env
  exprCode <- compileExpr e
  return $ exprCode ++ [Store v]
compileStmt (LRead s v) = do
  env <- get
  put $ Map.insert v (VInt 0) env
  return $ [Push (VStr s), Print, Input, Store v]
compileStmt (LPrint e) = do
  exprCode <- compileExpr e
  return $ exprCode ++ [Print]
compileStmt (Def v e stmts) = do
  env <- get
  put $ Map.insert v (exprType e) env
  exprCode <- compileExpr e
  bodyCode <- concatMapM compileStmt stmts
  put env
  return $ exprCode ++ [Store v] ++ bodyCode

-- Compile an expression
compileExpr :: E2 -> V1Compiler Code
compileExpr (LVal v) = return [Push v]
compileExpr (LVar v) = do
  env <- get
  case Map.lookup v env of
    Just _ -> return [Load v]
    Nothing -> throwError $ "Undefined variable: " ++ varName v
compileExpr (LAdd e1 e2) = do
  t1 <- exprTypeM e1
  t2 <- exprTypeM e2
  e1Code <- compileExpr e1
  e2Code <- compileExpr e2
  case (t1, t2) of
    (VInt _, VInt _) -> return $ e1Code ++ e2Code ++ [Add]
    (VStr _, VInt _) -> return $ e1Code ++ e2Code ++ [Concat]
    (VInt _, VStr _) -> return $ e1Code ++ e2Code ++ [Concat]
    (VStr _, VStr _) -> return $ e1Code ++ e2Code ++ [Concat]
compileExpr (LMinus e1 e2) = do
  t1 <- exprTypeM e1
  t2 <- exprTypeM e2
  e1Code <- compileExpr e1
  e2Code <- compileExpr e2
  case (t1, t2) of
    (VInt _, VInt _) -> return $ e1Code ++ e2Code ++ [Sub]
    _ -> throwError "Minus operation only supported for integers"
compileExpr (LMul e1 e2) = do
  t1 <- exprTypeM e1
  t2 <- exprTypeM e2
  e1Code <- compileExpr e1
  e2Code <- compileExpr e2
  case (t1, t2) of
    (VInt _, VInt _) -> return $ e1Code ++ e2Code ++ [Mul]
    _ -> throwError "Multiplication operation only supported for integers"
compileExpr (LDiv e1 e2) = do
  t1 <- exprTypeM e1
  t2 <- exprTypeM e2
  e1Code <- compileExpr e1
  e2Code <- compileExpr e2
  case (t1, t2) of
    (VInt _, VInt _) -> return $ e1Code ++ e2Code ++ [Div]
    _ -> throwError "Division operation only supported for integers"

-- Helper to infer expression type (pure, as environment is in State)
exprType :: E2 -> Value
exprType (LVal v) = v
exprType (LVar v) = VInt 0 -- Default, checked in compileExpr
exprType (LAdd e1 e2) = case (exprType e1, exprType e2) of
  (VInt _, VInt _) -> VInt 0
  (VStr _, _) -> VStr ""
  (_, VStr _) -> VStr ""
exprType (LMinus _ _) = VInt 0
exprType (LMul _ _) = VInt 0
exprType (LDiv _ _) = VInt 0

-- Monadic version of exprType for compileExpr
exprTypeM :: E2 -> V1Compiler Value
exprTypeM (LVal v) = return v
exprTypeM (LVar v) = do
  env <- get
  case Map.lookup v env of
    Just val -> return val
    Nothing -> throwError $ "Undefined variable: " ++ varName v
exprTypeM (LAdd e1 e2) = do
  t1 <- exprTypeM e1
  t2 <- exprTypeM e2
  return $ case (t1, t2) of
    (VInt _, VInt _) -> VInt 0
    (VStr _, _) -> VStr ""
    (_, VStr _) -> VStr ""
exprTypeM (LMinus _ _) = return $ VInt 0
exprTypeM (LMul _ _) = return $ VInt 0
exprTypeM (LDiv _ _) = return $ VInt 0

-- Helper to concatenate code from multiple statements
concatMapM :: (a -> V1Compiler Code) -> [a] -> V1Compiler Code
concatMapM f xs = concat <$> mapM f xs

-- Extract variable name
varName :: Var -> String
varName (Var s) = s