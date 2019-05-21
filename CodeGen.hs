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

genExpr (RelExpr _ relop e1 e2) r procTable (Just tLabel) (Just fLabel)
  = do
      (e1Type, e1Instrs) <- genExpr e1 e1Place procTable Nothing Nothing
      (e2Type, e2Instrs) <- genExpr e2 e2Place procTable Nothing Nothing

      conv <- genIntToReal e1Place e2Place e1Type e2Type

      let isFloat = e1Type == FloatType || e2Type == FloatType 
          op = case relop of
                 OpEq -> if isFloat then EqReal else EqInt
                 OpNe -> if isFloat then NeReal else NeInt
                 OpGe -> if isFloat then GeReal else GeInt
                 OpLe -> if isFloat then LeReal else LeInt
                 OpGt -> if isFloat then GtReal else GtInt
                 OpLt -> if isFloat then LtReal else LtInt
        
          binopExpr = [BinopInstr op e1Place e1Place e2Place]

      gotoE1True <- genBranchOnTrue tLabel e1Place
      gotoE1False <- genUncond fLabel

      let instrs = e1Instrs ++ e2Instrs ++ conv ++ binopExpr ++ gotoE1True ++ 
                   gotoE1False

      return (BoolType, instrs)

  where
    (Reg ePlace) = r
    e1Place = Reg ePlace
    e2Place = Reg (ePlace + 1)

genExpr (RelExpr _ relop e1 e2) r procTable Nothing Nothing
  = do
      (e1Type, e1Instrs) <- genExpr e1 e1Place procTable Nothing Nothing
      (e2Type, e2Instrs) <- genExpr e2 e2Place procTable Nothing Nothing

      conv <- genIntToReal e1Place e2Place e1Type e2Type

      let isFloat = e1Type == FloatType || e2Type == FloatType 
          op = case relop of
                 OpEq -> if isFloat then EqReal else EqInt
                 OpNe -> if isFloat then NeReal else NeInt
                 OpGe -> if isFloat then GeReal else GeInt
                 OpLe -> if isFloat then LeReal else LeInt
                 OpGt -> if isFloat then GtReal else GtInt
                 OpLt -> if isFloat then LtReal else LtInt
        
          binopExpr = [BinopInstr op e1Place e1Place e2Place]

          instrs = e1Instrs ++ e2Instrs ++ conv ++ binopExpr
       
      return (BoolType, instrs)     

  where
    (Reg ePlace) = r
    e1Place = Reg ePlace
    e2Place = Reg (ePlace + 1)


genExpr (BinopExpr _ binop e1 e2) r procTable _ _
  = do
      (e1Type, e1Instrs) <- genExpr e1 e1Place procTable Nothing Nothing
      (e2Type, e2Instrs) <- genExpr e2 e2Place procTable Nothing Nothing
        
      conv <- genIntToReal e1Place e2Place e1Type e2Type
                               
      let isFloat = e1Type == FloatType || e2Type == FloatType
          op = case binop of 
                 OpAdd -> if isFloat then AddReal else AddInt
                 OpSub -> if isFloat then SubReal else SubInt
                 OpMul -> if isFloat then MulReal else MulInt
                 OpDiv -> if isFloat then DivReal else DivInt
           
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

genExpr (UMinus _ expr) r procTable _ _
  = do 
      (exprType, exprInstrs) <- genExpr expr r procTable Nothing Nothing
      
      let isFloat = exprType == FloatType
          op = if isFloat then NegReal else NegInt
       
          instrs = exprInstrs ++ [UnopInstr op r r]
    
      return (exprType, instrs)
    

genStmt :: (GlobalSymTable, ProcSymTable) -> Stmt -> Codegen [Instr]

genStmt (table, pTable) (Assign _ lvalue expr)
  = do 
      let (VarInfo baseType _ slot) = fromJust $ getVarInfo ident pTable
      (exprType, exprInstrs) <- genExpr expr exprPlace pTable Nothing Nothing
      return $ exprInstrs ++ [Store slot exprPlace]

  where
    exprPlace = Reg 0

    ident = case lvalue of
              (LId _ ident) -> ident
              (LArrayRef _ ident _) -> ident
              (LMatrixRef _ ident _ _) -> ident

genStmt (table, pTable) (Read _ lvalue)
  = do
      let ident = case lvalue of
            LId _ i -> i
            LArrayRef _ i _ -> i
            LMatrixRef _ i _ _ -> i
          (VarInfo baseType _ s) = fromJust $ getVarInfo ident pTable
      
          readInstr = case baseType of
                        IntType -> [CallBuiltin ReadInt]
                        FloatType -> [CallBuiltin ReadReal]
                        BoolType -> [CallBuiltin ReadBool]

          storeInstr = [Store s (Reg 0)]

      return $ readInstr ++ storeInstr

genStmt (table, pTable) (Write _ expr)
  = do
      (baseType, exprInstrs) <- genExpr expr exprPlace pTable Nothing Nothing
      let builtin = case baseType of
                      BoolType -> PrintBool
                      IntType -> PrintInt
                      FloatType -> PrintReal
                      StringType -> PrintString
      return $ exprInstrs ++ [CallBuiltin builtin]

  where
    exprPlace = Reg 0


genStmt (table, pTable) (If _ expr stmts)
  = do
      eTrueLabel <- getLabelCounter
      incLabelCounter
      sAfterLabel <- getLabelCounter
      incLabelCounter
      let ePlace = Reg 0
          eTrue = show eTrueLabel
          sAfter = show sAfterLabel

      (eType, eInstrs) <- genExpr expr ePlace pTable (Just eTrue) (Just sAfter)

      eTrueInstr <- genLabel eTrue
      sInstrs <- mapM (genStmt (table, pTable)) stmts
      sAfterInstr <- genLabel sAfter
      
      let instrs = eInstrs ++ eTrueInstr ++ (concat sInstrs) ++ sAfterInstr

      return instrs
      
genStmt (table, pTable) (IfElse _ expr s1 s2)
  = do
      eFalseLabel <- getLabelCounter
      incLabelCounter
      eTrueLabel <- getLabelCounter
      incLabelCounter
      sAfterLabel <- getLabelCounter
      incLabelCounter
      
      let ePlace = Reg 0
          eFalse = show eFalseLabel
          eTrue = show eTrueLabel
          sAfter = show sAfterLabel
    
      (eType, eInstrs) <- genExpr expr ePlace pTable (Just eTrue) (Just eFalse)
      eTrueInstr <- genLabel eTrue
      s1Instrs <- mapM (genStmt (table, pTable)) s1
      gotoSAfter <- genUncond sAfter
      eFalseInstr <- genLabel eFalse
      s2Instrs <- mapM (genStmt (table, pTable)) s2
      sAfterInstr <- genLabel sAfter

      let instrs = eInstrs ++ eTrueInstr ++ (concat s1Instrs) ++ gotoSAfter ++ 
                   eFalseInstr ++ (concat s2Instrs) ++ sAfterInstr

      return instrs


genStmt (table, pTable) (While _ expr stmts) 
  = do
      sBeginLabel <- getLabelCounter
      incLabelCounter
      sBodyLabel <- getLabelCounter
      incLabelCounter
      sAfterLabel <- getLabelCounter
      incLabelCounter
      
      let ePlace = Reg 0
          sBegin = show sBeginLabel
          sBody = show sBodyLabel
          sAfter = show sAfterLabel
          eTrue = sBody
          eFalse = sAfter
      
      sBeginInstr <- genLabel sBegin   
      (eType, eInstrs) <- genExpr expr ePlace pTable (Just eTrue) (Just eFalse)
      sBodyInstr <- genLabel sBody
      sInstrs <- mapM (genStmt (table, pTable)) stmts
      gotoSBegin <- genUncond sBegin
      sAfterInstr <- genLabel sAfter

      let instrs = sBeginInstr ++ eInstrs ++ sBodyInstr ++ (concat sInstrs) ++
                   gotoSBegin ++ sAfterInstr

      return instrs
      

genProc :: GlobalSymTable -> Proc -> Codegen ProcCode
genProc table (Proc _ ident _ _ stmts)
  = do
      let (ProcInfo _ procTable) = fromJust $ getProcInfo ident table 
          stackFrameSize = getSize procTable
          epilogue = [PushSF stackFrameSize]
          prologue = [PopSF stackFrameSize, Return]
      stmtInstrs <- mapM (genStmt (table, procTable)) stmts
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
