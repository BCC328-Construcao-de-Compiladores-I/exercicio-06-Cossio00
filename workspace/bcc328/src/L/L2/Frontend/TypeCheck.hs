module L.L2.Frontend.TypeCheck where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import Data.List ((\\))
import L.L2.Frontend.Syntax
import Utils.Var
import Utils.Value
import Text.Printf
import Data.Map (Map)
import qualified Data.Map as Map

-- Semantic environment: maps variables to their types
type TcEnv = Map Var Value

-- Type checker monad: errors, warnings, and state
type TcM a = ExceptT String (WriterT [String] (StateT TcEnv Identity)) a

initTcEnv :: TcEnv
initTcEnv = Map.empty

insertVar :: Var -> Value -> TcM ()
insertVar v t = modify (\env -> Map.insert v t env)

removeVar :: Var -> TcM ()
removeVar v = modify (\env -> Map.delete v env)

runTcM :: TcEnv -> TcM a -> (((Either String a), [String]), TcEnv)
runTcM env m = runIdentity (runStateT (runWriterT (runExceptT m)) env)

typeCheck :: L2 -> Either String L2
typeCheck p@(L2 stmts) = case runTcM initTcEnv (mapM_ checkStmt stmts) of
  (((Left err), warns), _) -> Left $ err ++ if null warns then "" else "\nWarnings: " ++ unlines warns
  (((Right _), warns), _) -> Right p

checkStmt :: S2 -> TcM ()
checkStmt (LAssign v e) = do
  t <- checkExpr e
  insertVar v t
checkStmt (LRead s v) = do
  t <- checkExpr (LVal (VStr s))
  case t of
    VStr _ -> insertVar v (VInt 0) 
    _ -> throwError $ "Type error: read prompt must be a string, got " ++ show t
checkStmt (LPrint e) = do
  t <- checkExpr e
  case t of
    VInt _ -> return ()
    VStr _ -> return ()
    VBool _ -> return ()
checkStmt (Def v e stmts) = do
  env <- get
  t <- checkExpr e
  insertVar v t
  mapM_ checkStmt stmts
  put env 

checkExpr :: E2 -> TcM Value
checkExpr (LVal v) = return v
checkExpr (LVar v) = do
  env <- get
  case Map.lookup v env of
    Just t -> return t
    Nothing -> throwError $ "Undefined variable: " ++ varName v
checkExpr (LAdd e1 e2) = do
  t1 <- checkExpr e1
  t2 <- checkExpr e2
  case (t1, t2) of
    (VStr _, VStr _) -> case catValue t1 t2 of
      Right t -> return t
      Left err -> throwError err
    (VStr _, VInt _) -> case i2s t2 of
      Right (VStr s2) -> return $ VStr (s1 ++ s2)
        where VStr s1 = t1
      Left err -> throwError err
    (VInt _, VStr _) -> case i2s t1 of
      Right (VStr s1) -> return $ VStr (s1 ++ s2)
        where VStr s2 = t2
      Left err -> throwError err
    (VInt _, VInt _) -> case t1 .+. t2 of
      Right t -> return t
      Left err -> throwError err
    _ -> throwError $ "Type error in +: " ++ show t1 ++ ", " ++ show t2
checkExpr (LMinus e1 e2) = do
  t1 <- checkExpr e1
  t2 <- checkExpr e2
  case t1 .-. t2 of
    Right t -> return t
    Left err -> throwError err
checkExpr (LMul e1 e2) = do
  t1 <- checkExpr e1
  t2 <- checkExpr e2
  case t1 .*. t2 of
    Right t -> return t
    Left err -> throwError err
checkExpr (LDiv e1 e2) = do
  t1 <- checkExpr e1
  t2 <- checkExpr e2
  case t2 of
    VInt 0 -> tell ["Warning: Potential division by zero in " ++ show e1 ++ " / " ++ show e2]
    _ -> return ()
  case t1 ./. t2 of
    Right t -> return t
    Left err -> throwError err

varName :: Var -> String
varName (Var s) = s