--  Vinicius Cossio de Oliveira 19.1.4004
module L.L2.Frontend.AST (toTree) where

import Data.Tree (Tree(..))
import L.L2.Frontend.Syntax (L2(..), S2(..), E2(..))
import Utils.Value (Value(..))
import Utils.Var (Var(..))
import Utils.Pretty (pretty)

-- Converte uma AST L2 em uma árvore para visualização com Data.Tree
-- Para gerar este código utilizei como base o código do semestre passado feito por mim e pela minha dupla da época, o Gabriel
toTree :: L2 -> Tree String
toTree (L2 stmts) = Node "L2" (map toTreeStmt stmts)

toTreeStmt :: S2 -> Tree String
toTreeStmt (LAssign v e) = Node ("LAssign " ++ pretty v) [toTreeExpr e]
toTreeStmt (LRead s v) = Node ("LRead \"" ++ s ++ "\" " ++ pretty v) []
toTreeStmt (LPrint e) = Node "LPrint" [toTreeExpr e]
toTreeStmt (Def v e stmts) = Node ("Def " ++ pretty v) [toTreeExpr e, Node "stmts" (map toTreeStmt stmts)]

toTreeExpr :: E2 -> Tree String
toTreeExpr (LVal (VInt n)) = Node ("LVal (VInt " ++ show n ++ ")") []
toTreeExpr (LVal (VStr s)) = Node ("LVal (VStr \"" ++ s ++ "\")") []
toTreeExpr (LVar v) = Node ("LVar " ++ pretty v) []
toTreeExpr (LAdd e1 e2) = Node "LAdd" [toTreeExpr e1, toTreeExpr e2]
toTreeExpr (LMinus e1 e2) = Node "LMinus" [toTreeExpr e1, toTreeExpr e2]
toTreeExpr (LMul e1 e2) = Node "LMul" [toTreeExpr e1, toTreeExpr e2]
toTreeExpr (LDiv e1 e2) = Node "LDiv" [toTreeExpr e1, toTreeExpr e2]