module CodeGen (genCode) where

import GoatAST
import GoatIR
import SymTable

-------------------------------------------------------------------------------
-- code in this block generates intermediate representations for the target 
-- language for expressions
-------------------------------------------------------------------------------
genExpr :: Expr -> Reg -> Maybe Slot -> [Instr]
genExpr (StrConst _ val) r _
  = [StringConst r val]



genStmt :: Stmt -> [Instr]
genStmt (Write _ expr) 
  = genExpr expr (Reg 0) Nothing ++ callBuiltin
  where
    callBuiltin 
      = case expr of
          StrConst _ _ -> [CallBuiltin PrintString]

genProc :: Proc -> ProcCode
genProc (Proc _ ident _ _ stmts)
  = ProcCode ident $ concat $ map genStmt stmts


-- Might need to change label 
-- Maybe dont need
genInit:: [Instr]
genInit
  = [Call "proc_main", Halt]

genCode :: GoatProgram -> GlobalSymTable -> [ProcCode]
genCode (GoatProgram procs) table
  = map genProc procs
