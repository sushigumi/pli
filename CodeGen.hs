module CodeGen (genCode) where

import GoatAST
import GoatIR
import SymTable
import Data.Maybe
import Control.Monad.State

type LabelCounter = Int

type Codegen a
  = State LabelCounter a

getLabelCounter :: Codegen LabelCounter
getLabelCounter 
  = do
      label <- get
      return label

incLabelCounter :: Codegen ()
incLabelCounter
  = do
      label <- get
      put (label + 1)
      return ()


genLabel :: Label -> Codegen [Instr]
genLabel label
  = return [LabelI label]

genBranchOnTrue :: Label -> Reg -> Codegen [Instr]
genBranchOnTrue label r 
  = return [BranchOnTrue r label] 

genBranchOnFalse :: Label -> Reg -> Codegen [Instr]
genBranchOnFalse label r
  = return [BranchOnFalse r label]

genUncond :: Label -> Codegen [Instr]
genUncond label
  = return [BranchUncond label]


genIntToReal :: Reg -> Reg -> BaseType -> BaseType -> Codegen [Instr]
genIntToReal r1 r2 b1 b2
  | b1 == IntType && b2 == FloatType = return [IntToReal r1 r1]
  | b2 == IntType && b1 == FloatType = return [IntToReal r2 r2]
  | otherwise = return []


genExpr :: Expr -> Reg -> ProcSymTable -> Maybe Label -> Maybe Label 
           -> Codegen (BaseType, [Instr])
genExpr (BoolConst _ val) r _ _ _
  | val == True =  return (BoolType, [IntConstI r 1])
  | val == False = return (BoolType, [IntConstI r 0])

genExpr (IntConst _ val) r _ _ _
  = return (IntType, [IntConstI r val])

genExpr (FloatConst _ val) r _ _ _
  = return (FloatType, [RealConstI r val])

genExpr (StrConst _ val) r _ _ _
  = return (StringType, [StringConstI r val])

genExpr (Id pos ident) r procTable _ _
  = do
      let (VarInfo baseType _ s) = fromJust $ getVarInfo ident procTable 
      return (baseType, [Load r s])

-- Maybe handle nothing here so that nothing means there is no cond
genExpr (And _ e1 e2) r procTable (Just tLabel) (Just fLabel)
  = do
      e1TrueLabel <- getLabelCounter
      incLabelCounter
      let e1Place = r
          e2Place = r
          e1False = fLabel
          e1True = show e1TrueLabel
      let e2False = fLabel
          e2True = tLabel
      (e1Type, e1Instrs) <- genExpr e1 r procTable (Just e1True) (Just e1False)
      gotoE1True <- genBranchOnTrue e1True r
      gotoE1False <- genUncond e1False
      e1TrueInstrs <- genLabel e1True
      (e2Type, e2Instrs) <- genExpr e2 r procTable (Just e2True) (Just e2False)
      gotoE2True <- genBranchOnTrue e2True r
      gotoE2False <- genUncond e2False
      
      let instrs = e1Instrs ++ gotoE1True ++ gotoE1False ++ e1TrueInstrs ++ 
                   e2Instrs ++ gotoE2True ++ gotoE2False
    
      return (BoolType, instrs)


genExpr (And _ e1 e2) r procTable Nothing Nothing
  = do
      (e1Type, e1Instrs) <- genExpr e1 e1Place procTable Nothing Nothing
      (e2Type, e2Instrs) <- genExpr e2 e2Place procTable Nothing Nothing
      let instrs = e1Instrs ++ e2Instrs ++ [BinopInstr AndI r e1Place e2Place]
      return $ (BoolType, instrs)
  where
    (Reg ePlace) = r
    e1Place = Reg ePlace
    e2Place = Reg (ePlace + 1)


genExpr (Or _ e1 e2) r procTable (Just tLabel) (Just fLabel)
  = do
      e1FalseLabel <- getLabelCounter
      incLabelCounter
      let e1Place = r
          e2Place = r
          e1False = show e1FalseLabel
          e1True = tLabel
          e2False = fLabel
          e2True = tLabel
      (e1Type, e1Instrs) <- genExpr e1 r procTable (Just e1True) (Just e1False)
      gotoE1True <- genBranchOnTrue e1True e1Place
      gotoE1False <- genUncond e1False 
      e1FalseInstrs <- genLabel e1False
      (e2Type, e2Instrs) <- genExpr e2 r procTable (Just e2True) (Just e2False)
      gotoE2True <- genBranchOnTrue e2True e2Place
      gotoE2False <- genUncond e2False
    
      let instrs = e1Instrs ++ gotoE1True ++ gotoE1False ++ e1FalseInstrs ++ 
                   e2Instrs ++ gotoE2True ++ gotoE2False
    
      return (BoolType, instrs)
     
      
genExpr (Or _ e1 e2) r procTable Nothing Nothing
  = do
      (e1Type, e1Instrs) <- genExpr e1 e1Place procTable Nothing Nothing
      (e1Type, e2Instrs) <- genExpr e2 e2Place procTable Nothing Nothing
      let instrs = e1Instrs ++ e2Instrs ++ [BinopInstr OrI r e1Place e2Place]
      return (BoolType, instrs)
      
  where
    (Reg ePlace) = r
    e1Place = Reg ePlace 
    e2Place = Reg (ePlace + 1)
    
      
genExpr (Not _ expr) r procTable (Just tLabel) (Just fLabel)
 = do
     let eFalse = tLabel
         eTrue = fLabel
     (eType, eInstrs) <- genExpr expr r procTable (Just eTrue) (Just eFalse)
     return (BoolType, eInstrs)

genExpr (Not _ expr) r procTable Nothing Nothing
  = do
      (eType, eInstrs) <- genExpr expr r procTable Nothing Nothing
      let instrs = eInstrs ++ [UnopInstr NotI r r]
      return (BoolType, instrs)


genExpr (BinopExpr _ binop e1 e2) r procTable _ _
  = do
      (e1Type, e1Instrs) <- genExpr e1 e1Place procTable Nothing Nothing
      (e2Type, e2Instrs) <- genExpr e2 e2Place procTable Nothing Nothing
        
      conv <- genIntToReal e1Place e2Place e1Type e2Type
                               
      let op = case binop of 
                 OpAdd -> if e1Type == FloatType || e2Type == FloatType then
                            AddReal
                          else 
                            AddInt
                 OpSub -> if e1Type == FloatType || e2Type == FloatType then
                            SubReal
                          else
                            SubInt
                 OpMul -> if e1Type == FloatType || e2Type == FloatType then
                            MulReal
                          else
                            MulInt
                 OpDiv -> if e1Type == FloatType || e2Type == FloatType then
                            DivReal
                          else
                            DivInt
           
          binopInstr = [BinopInstr op e1Place e1Place e2Place]

          instrs = e1Instrs ++ e2Instrs ++ conv ++ binopInstr
    
          exprType = if e1Type == FloatType || e2Type == FloatType then
                       FloatType
                     else
                       e1Type
    
      return (exprType, instrs)
  where
    (Reg ePlace) = r
    e1Place = Reg ePlace
    e2Place = Reg (ePlace + 1)


genStmt :: ProcSymTable -> Stmt -> Codegen [Instr]

genStmt table (Assign _ lvalue expr)
  = do 
      let (VarInfo baseType _ slot) = fromJust $ getVarInfo ident table
      (exprType, exprInstrs) <- genExpr expr exprPlace table Nothing Nothing
      return $ exprInstrs ++ [Store slot exprPlace]

  where
    exprPlace = Reg 0

    ident = case lvalue of
              (LId _ ident) -> ident
              (LArrayRef _ ident _) -> ident
              (LMatrixRef _ ident _ _) -> ident

genStmt table (Write _ expr)
  = do
      (baseType, exprInstrs) <- genExpr expr exprPlace table Nothing Nothing
      let builtin = case baseType of
                      BoolType -> PrintBool
                      IntType -> PrintInt
                      FloatType -> PrintReal
                      StringType -> PrintString
      return $ exprInstrs ++ [CallBuiltin builtin]

  where
    exprPlace = Reg 0


genProc :: GlobalSymTable -> Proc -> Codegen ProcCode
genProc table (Proc _ ident _ _ stmts)
  = do
      let (ProcInfo _ procTable) = fromJust $ getProcInfo ident table 
          stackFrameSize = getSize procTable
          epilogue = [PushSF stackFrameSize]
          prologue = [PopSF stackFrameSize, Return]
      stmtInstrs <- mapM (genStmt procTable) stmts
      return $ ProcCode ident (epilogue ++ (concat stmtInstrs) ++ prologue)

genProcs :: GlobalSymTable -> [Proc] -> Codegen [ProcCode]
genProcs table procs
  = do
      procCodes <- mapM (genProc table) procs
      return procCodes

genCode :: GoatProgram -> GlobalSymTable -> [ProcCode]
genCode (GoatProgram procs) table
  = do 
      evalState (genProcs table procs) startState
  where
    startState = 0
