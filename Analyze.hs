module Analyze where

import GoatAST
import SymTable

type AnnAST a = State Symtable a

aExpr :: Expr -> Node


aStmts :: [Stmt] -> AnnAst [Node]
aStmts []
  = return get
aStmts (s:stmts)
  = do
      aStmt s
      aStmts stmts

aStmt :: Stmt -> Node
aStmt (Write expr) 
  = StmtNode exprt

-- TODO add some checking for arg length of main
aFunc :: Func -> AnnAST [Func]
aFunc (Func "main" args decls (s:stmts))
  = do
      aStmt s
      aStmts
  where 
    nodes = 

analyze :: GoatProgram -> AnnGoatProgram
analyze (GoatProgram funcs)
  = AnnGoatProgram $ map aFunc funcs

