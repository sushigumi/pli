module CodeGen (genCode) where

import GoatAST
import GoatIR
import SymTable
import System.Exit

-------------------------------------------------------------------------------
-- code in this block generates intermediate representations for the target 
-- language for expressions
-------------------------------------------------------------------------------
genExpr :: Expr -> Reg -> ProcSymTable -> IO (BaseType, [Instr])
genExpr (BoolConst _ val) r _ 
  | val == True =  return (BoolType, [IntConstI r 1])
  | val == False = return (BoolType, [IntConstI r 0])
genExpr (IntConst _ val) r _ 
  = return (IntType, [IntConstI r val])
genExpr (FloatConst _ val) r _ 
  = return (FloatType, [RealConstI r val])
genExpr (StrConst _ val) r _
  = return (StringType, [StringConstI r val])
genExpr (Id pos ident) r procTable
  = do
      case getVarInfo ident procTable of
        Just (VarInfo baseType _ s) -> return (baseType, [Load r s])
        Nothing      -> do
                          exitWith (ExitFailure 4)

-- OTherwise error occured due to invalid format


genStmt :: ProcSymTable -> Stmt -> IO [Instr]

genStmt procTable (Assign _ lvalue expr)
  = do
      case getVarInfo ident procTable of
        Just (VarInfo varType _ slot) 
          -> do
               (exprType, exprInstr) <- genExpr expr exprPlace procTable
-- Add checking of type here exprType and varType 
               let storeInstr = [Store slot exprPlace]
               return (exprInstr ++ storeInstr)
        Nothing 
          -> do
               exitWith (ExitFailure 4)
  where
    exprPlace = Reg 0

    ident = case lvalue of
              (LId _ ident) -> ident
              (LArrayRef _ ident _) -> ident
              (LMatrixRef _ ident _ _) -> ident


genStmt procTable (Write _ expr) 
  = do
      (baseType, exprInstr) <- genExpr expr (Reg 0) procTable
      let builtin = case baseType of
                      BoolType -> PrintBool
                      IntType -> PrintInt
                      FloatType -> PrintReal
                      StringType -> PrintString
      return (exprInstr ++ [CallBuiltin builtin])

    

genProc :: GlobalSymTable -> Proc -> IO ProcCode
genProc globalTable (Proc _ ident _ _ stmts) 
  = do 
      case getProcInfo ident globalTable of
        (Just (ProcInfo _ procTable)) 
          -> do
               let stackFrameSize = getSize procTable
                   epilogue = [PushSF stackFrameSize]
                   prologue = [PopSF stackFrameSize, Return]
               stmtInstrs <- mapM (genStmt procTable) stmts
               return (ProcCode ident (epilogue ++ (concat stmtInstrs) ++ 
                      prologue))
        Nothing
          -> exitWith (ExitFailure 4)

-- Might need to change label 
-- Maybe dont need
genInit:: [Instr]
genInit
  = [Call "proc_main", Halt]

genCode :: GoatProgram -> GlobalSymTable -> IO [ProcCode]
genCode (GoatProgram procs) table
  = do 
      procCodes <- mapM (genProc table) procs
      return procCodes
