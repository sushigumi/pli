module Analyse where

import SymTable
import qualified Data.Map.Strict as Map
import GoatAST
import CodeGen
import GoatIR

--aExpr :: Expr -> GlobalSymTable -> Env -> Reg -> [Instr]
--aExpr (StrConst pos str) table e reg
--  = genStrConst reg str
--
--
--aStmt :: Stmt -> GlobalSymTable -> Env -> [Instr]
--aStmt (Write pos expr) table e
--  = eCode ++ genWriteStmt  
--  where
--    eCode = aExpr expr table e (Reg 0)
--
--aStmts :: [Stmt] -> GlobalSymTable -> [Instr] -> Env -> [Instr]
--aStmts [] _ instrs env
--  = instrs 
--aStmts (s:stmts) globalTable instrs env
--  = aStmts stmts globalTable instrs1 env
--  where
--    instrs1 = instrs ++ (aStmt s globalTable env)
--
--aProc :: Pos -> Ident -> [Stmt] -> GlobalSymTable -> [Instr]
--aProc pos ident stmts table
--  = aStmts stmts table [] ident
--
--aProcs :: [Proc] -> GlobalSymTable -> [ProcCode]
--aProcs [] _ 
--  = []
--aProcs ((Proc pos ident _ _ stmts):procs) table
--  = (ProcCode ident procCode) : aProcs procs table
--  where 
--    procCode = aProc pos ident stmts table 

-- Analyse all the functions first to store all the parameters and names into
-- the symbol table before proceeding to each function's inside semantic 
-- analysis
initAnalyse :: [Proc] -> GlobalSymTable -> GlobalSymTable
initAnalyse [] table
  = table
initAnalyse ((Proc pos ident args decls stmts):procs) table
  = initAnalyse procs table1
  where
-- Probably print an error for Nothing
    procSymTable 
      = case getProcInfo ident table of
          Just (ProcInfo _ procTable) -> procTable
          Nothing -> Map.empty

    newProcSymTable = insertVars decls 0 procSymTable

    table1 = updateProcSymTable ident args newProcSymTable table

analyse :: GoatProgram -> GlobalSymTable
analyse (GoatProgram procs)
  = globalTable
  where
    startTable = initGlobalTable procs

    globalTable = initAnalyse procs startTable
