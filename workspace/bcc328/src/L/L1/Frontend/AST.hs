--  Vinicius Cossio de Oliveira 19.1.4004
module L.L1.Frontend.AST (toTree) where

import Data.Tree (Tree(..))
import L.L1.Frontend.Syntax (L1(..), S1(..), E1(..))
import Utils.Value (Value(..))
import Utils.Var (Var(..))
import Utils.Pretty (pretty)

-- Converte uma AST L1 em uma árvore para visualização com Data.Tree
-- Para gerar este código utilizei como base o código do semestre passado feito por mim e pela minha dupla da época, o Gabriel
toTree :: L1 -> Tree String
toTree (L1 stmts) = Node "L1" (map toTreeStmt stmts)

toTreeStmt :: S1 -> Tree String
toTreeStmt (LAssign v e) = Node ("LAssign " ++ pretty v) [toTreeExpr e]
toTreeStmt (LRead s v) = Node ("LRead \"" ++ s ++ "\" " ++ pretty v) []
toTreeStmt (LPrint e) = Node "LPrint" [toTreeExpr e]

toTreeExpr :: E1 -> Tree String
toTreeExpr (LVal (VInt n)) = Node ("LVal (VInt " ++ show n ++ ")") []
toTreeExpr (LVal (VStr s)) = Node ("LVal (VStr \"" ++ s ++ "\")") []
toTreeExpr (LVar v) = Node ("LVar " ++ pretty v) []
toTreeExpr (LAdd e1 e2) = Node "LAdd" [toTreeExpr e1, toTreeExpr e2]
toTreeExpr (LMinus e1 e2) = Node "LMinus" [toTreeExpr e1, toTreeExpr e2]
toTreeExpr (LMul e1 e2) = Node "LMul" [toTreeExpr e1, toTreeExpr e2]
toTreeExpr (LDiv e1 e2) = Node "LDiv" [toTreeExpr e1, toTreeExpr e2]