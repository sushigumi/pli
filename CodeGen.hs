module CodeGen (genCode) where

import GoatAST
import GoatIR
import SymTable

-------------------------------------------------------------------------------
-- code in this block generates intermediate representations for the target 
-- language for expressions
-------------------------------------------------------------------------------
genExpr :: Expr -> Reg -> Maybe Slot -> [Instr]
genExpr (BoolConst _ val) r _ 
  | val == True =  [IntConstI r 1]
  | val == False = [IntConstI r 0]
genExpr (IntConst _ val) r _ 
  = [IntConstI r val]
genExpr (FloatConst _ val) r _ 
  = [RealConstI r val]
genExpr (StrConst _ val) r _
  = [StringConstI r val]



genStmt :: Stmt -> [Instr]
genStmt (Write _ expr) 
  = genExpr expr (Reg 0) Nothing ++ callBuiltin
  where
    callBuiltin 
      = case expr of
          BoolConst _ _ -> [CallBuiltin PrintBool]
          IntConst _ _ -> [CallBuiltin PrintInt]
          FloatConst _ _ -> [CallBuiltin PrintReal]
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
