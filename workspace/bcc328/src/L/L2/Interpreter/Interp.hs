module L.L2.Interpreter.Interp where

import L.L2.Frontend.Syntax
import Utils.Value
import Utils.Var
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import System.IO (hFlush, stdout)
import Debug.Trace (trace)

type Env = Map.Map Var Value
type Store = Map.Map Var Int
type Output = [String]
type I a = ReaderT Env (StateT (Store, Output) IO) a

evalExpr :: E2 -> I Value
evalExpr (LVal v) = return v
evalExpr (LVar v) = do
  env <- ask
  store <- lift $ gets fst
  liftIO $ putStrLn $ "Looking up " ++ show v ++ " in env: " ++ show env ++ ", store: " ++ show store
  case Map.lookup v env of
    Just val -> return val
    Nothing -> case Map.lookup v store of
      Just n -> return (VInt n)
      Nothing -> error $ "Undefined variable: " ++ show v
evalExpr (LAdd e1 e2) = do
  v1 <- evalExpr e1
  v2 <- evalExpr e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> return (VInt (n1 + n2))
    (VStr s, VInt n) -> return (VStr (s ++ show n))
    (VInt n, VStr s) -> return (VStr (show n ++ s))
    (VStr s1, VStr s2) -> return (VStr (s1 ++ s2))
evalExpr (LMinus e1 e2) = do
  VInt n1 <- evalExpr e1
  VInt n2 <- evalExpr e2
  return (VInt (n1 - n2))
evalExpr (LMul e1 e2) = do
  VInt n1 <- evalExpr e1
  VInt n2 <- evalExpr e2
  return (VInt (n1 * n2))
evalExpr (LDiv e1 e2) = do
  VInt n1 <- evalExpr e1
  VInt n2 <- evalExpr e2
  if n2 == 0
    then error "Division by zero"
    else return (VInt (n1 `div` n2))

execStmt :: S2 -> I ()
execStmt (LAssign v e) = do
  VInt n <- evalExpr e
  liftIO $ putStrLn $ "Assigning " ++ show v ++ " = " ++ show n
  lift $ modify $ \(store, out) -> (Map.insert v n store, out)
execStmt (LRead s v) = do
  liftIO $ putStr s
  liftIO $ hFlush stdout
  input <- liftIO getLine
  case reads input :: [(Int, String)] of
    [(n, "")] -> lift $ modify $ \(store, out) -> (Map.insert v n store, out)
    _ -> error $ "Invalid input for read: " ++ input
execStmt (LPrint e) = do
  v <- evalExpr e
  case v of
    VInt n -> lift $ modify $ \(store, out) -> (store, out ++ [show n])
    VStr s -> lift $ modify $ \(store, out) -> (store, out ++ [s])
execStmt (Def v e stmts) = do
  val <- evalExpr e
  local (Map.insert v val) $ mapM_ execStmt stmts

execProg :: L2 -> I ()
execProg (L2 stmts) = mapM_ execStmt stmts

interp :: L2 -> IO [String]
interp prog = do
  (_, (_, out)) <- runStateT (runReaderT (execProg prog) Map.empty) (Map.empty, [])
  return out