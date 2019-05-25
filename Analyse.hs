module Analyse where

import SymTable
import qualified Data.Map.Strict as Map
import GoatAST
import GoatIR
import System.Exit
import Data.Maybe

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

logError :: String -> Pos -> IO ()
logError str (line, col)
  = putStrLn $ str ++ " at line " ++ (show line) ++ " column " ++ (show col)

checkArrayExprType :: ProcSymTable -> Pos -> Expr -> IO ()
checkArrayExprType pTable pos e
  = do
      eType <- aExpr pTable e
      if eType /= IntType then
        do
          logError "array accessor should be of type int" pos
          exitWith (ExitFailure 4)
      else
        return ()

checkMatrixExprType :: ProcSymTable -> Pos -> Expr -> Expr -> IO ()
checkMatrixExprType pTable pos e1 e2
  = do
      e1Type <- aExpr pTable e1
      e2Type <- aExpr pTable e2

      if e1Type /= IntType && e2Type /= IntType then
        do
          logError "matrix accessor should be of type int" pos
          exitWith (ExitFailure 4)
      else
        return ()
 

aLvalue :: ProcSymTable -> Lvalue -> IO BaseType
aLvalue pTable (LId pos ident) 
  = do
      let idInfo = getVarInfo ident pTable
      case idInfo of
        Just (VarInfo bType _ _ info) 
          -> case info of
               IdInfo -> return bType
               _ -> do
                      logError (ident ++ " should be an identifier") pos
                      exitWith (ExitFailure 4)
        Nothing 
          -> do
               logError "undeclared variable" pos
               exitWith (ExitFailure 4)

aLvalue pTable (LArrayRef pos ident e)
  = do
      checkArrayExprType pTable pos e

      let arrayInfo = getVarInfo ident pTable
      case arrayInfo of
        Just (VarInfo bType _ _ info)
          -> case info of
               ArrayInfo _ -> return bType
               _ -> do
                      logError (ident ++ " should be an array") pos
                      exitWith (ExitFailure 4)
        Nothing 
          -> do
               logError "undeclared variable" pos
               exitWith (ExitFailure 4)

pLvalue pTable (LMatrixRef pos ident e1 e2)
  = do
      checkMatrixExprType pTable pos e1 e2
     
      let matrixInfo = getVarInfo ident pTable
      case matrixInfo of
        Just (VarInfo bType _ _ info)
          -> case info of
               MatrixInfo _ _ -> return bType
               _ -> do
                      logError (ident ++ " should be a matrix") pos
                      exitWith (ExitFailure 4)
        Nothing
          -> do
               logError "undeclared variable" pos
               exitWith (ExitFailure 4)

                      
aExpr :: ProcSymTable -> Expr -> IO BaseType
aExpr _ (BoolConst pos _)
  = return BoolType

aExpr _ (IntConst pos _)
  = return IntType

aExpr _ (FloatConst pos _)
  = return FloatType

aExpr _ (StrConst pos _)
  = return StringType

aExpr pTable (Id pos ident)
  = do
      let idInfo = getVarInfo ident pTable
      idType <- case idInfo of
                  Just (VarInfo bType _ _ _) -> return bType 
                  Nothing -> do
                               logError "undeclared variable" pos
                               exitWith (ExitFailure 4)

      return idType
      
aExpr pTable (ArrayRef pos ident expr)
  = do
      checkArrayExprType pTable pos expr
      let arrayInfo = getVarInfo ident pTable
      (aType, info) <- case arrayInfo of  
                         Just (VarInfo bType _ _ info) -> return (bType, info)
                         Nothing -> do
                                      logError "undeclared variable" pos
                                      exitWith (ExitFailure 4)

      case info of 
        ArrayInfo _ -> return aType
        _ -> do
               logError (ident ++ " should have array type") pos
               exitWith (ExitFailure 4)
        
aExpr pTable (MatrixRef pos ident e1 e2)
  = do
      e1Type <- aExpr pTable e1
      e2Type <- aExpr pTable e2

      if e1Type /= IntType && e2Type /= IntType then
        do
          logError "invalid array access" pos
          exitWith (ExitFailure 4)

      else  
        do
          checkMatrixExprType pTable pos e1 e2
          let matrixInfo = getVarInfo ident pTable
          case matrixInfo of
            Just (VarInfo bType _ _ info)
              -> case info of
                   MatrixInfo _ _ -> return bType
                   _ -> do
                          logError (ident ++ " should have matrix type") pos
                          exitWith (ExitFailure 4)
            Nothing -> do
                         logError "undeclared variable" pos
                         exitWith (ExitFailure 4)

aExpr pTable (And pos e1 e2)
  = do
      e1Type <- aExpr pTable e1
      e2Type <- aExpr pTable e2
    
      if e1Type /= BoolType && e2Type /= BoolType then
        do
          logError "binary operator '&&' requires two bool operands" pos
          exitWith (ExitFailure 4)

      else 
        return BoolType

aExpr pTable (Or pos e1 e2)
  = do
      e1Type <- aExpr pTable e1
      e2Type <- aExpr pTable e2
    
      if e1Type /= BoolType && e2Type /= BoolType then
        do
          logError "binary operator '||' requires two bool operands" pos
          exitWith (ExitFailure 4)
    
      else
        return BoolType

aExpr pTable (Not pos expr)
  = do
      eType <- aExpr pTable expr

      if eType /= BoolType then
        do 
          logError "unary operator '!' requires a bool operand" pos
          exitWith (ExitFailure 4)

      else
        return BoolType

aExpr pTable (RelExpr pos relop e1 e2)
  = do
      e1Type <- aExpr pTable e1
      e2Type <- aExpr pTable e2
      
      if relop == OpEq || relop == OpNe then
        if (e1Type /= e2Type) then 
          do 
            logError "invalid operand type" pos
            exitWith (ExitFailure 4)
        else
          return BoolType 
      else
        case e1Type of
          IntType -> if (e2Type /= IntType && e2Type /= FloatType) then 
                       do
                         logError "invalid operand type" pos
                         exitWith (ExitFailure 4)
                     else
                       return BoolType

          FloatType -> if (e2Type /= IntType && e2Type /= FloatType) then 
                         do
                           logError "invalid operand type" pos
                           exitWith (ExitFailure 4)
                       else 
                         return BoolType

          BoolType -> if (e2Type /= BoolType) then
                        do
                          logError "invalid operand types" pos
                          exitWith (ExitFailure 4)
                      else
                        return BoolType

      return BoolType

aExpr pTable (BinopExpr pos binop e1 e2)
  = do
      e1Type <- aExpr pTable e1
      e2Type <- aExpr pTable e2

      if (e1Type == BoolType && e2Type == BoolType) then
        do
          logError "bool not supported in binary operations" pos
          exitWith (ExitFailure 4)

      else if (e1Type == IntType) then
        if (e2Type == IntType) then 
          return IntType
        
        else if (e2Type == FloatType) then 
          return FloatType

        else 
          do
            logError errmsg pos
            exitWith (ExitFailure 4)

      else if e1Type == FloatType then
        if (e2Type == IntType) then 
          return FloatType

        else if (e2Type == FloatType) then 
          return FloatType                             
       
        else 
          do
            logError errmsg pos
            exitWith (ExitFailure 4)
      else 
        do
          logError "operands must be either type int or float" pos
          exitWith (ExitFailure 4)
  where 
    errmsg = "operands must be either type int or float"

aExpr pTable (UMinus pos expr)
  = do
      eType <- aExpr pTable expr
        
      if eType == BoolType then
        do
          logError "bool not supported in unary minus" pos
          exitWith (ExitFailure 4)

      else if eType == StringType then
        do
          logError "string not supported in unary minus" pos
          exitWith (ExitFailure 4)

      else 
        return eType

getLvalueType :: Maybe VarInfo -> Pos-> IO BaseType
getLvalueType (Just (VarInfo bType _ _ _)) pos
  = return bType
getLvalueType Nothing pos
  = do
      logError "undeclared variable" pos
      exitWith (ExitFailure 4)

aStmt :: (GlobalSymTable, ProcSymTable) -> Stmt -> IO ()
aStmt (table, pTable) (Assign pos lvalue expr) 
  = do
      let lvalueInfo = getVarInfo ident pTable

      lvalueType <- getLvalueType lvalueInfo pos
      eType <- aExpr pTable expr

      if eType /= lvalueType then
        if eType == IntType && lvalueType == FloatType then
          return ()
        else
          do
            logError "illegal assignment" pos
            exitWith (ExitFailure 4)

      else 
        return ()
  where
    ident = case lvalue of
              (LId _ i) -> i
              (LArrayRef _ i _) -> i
              (LMatrixRef _ i _ _) -> i

aStmt (table, pTable) (Read pos lvalue)
  = do
      aLvalue pTable lvalue
      return ()

aStmt (table, pTable) (Write pos expr) 
  = do
      eType <- aExpr pTable expr
      return ()

aStmt (table, pTable) (ProcCall pos ident exprs)
  = do
      eTypes <- mapM (aExpr pTable) exprs
      
      let procInfo = getProcInfo ident table
      case procInfo of 
        Just (ProcInfo args _) 
          -> do 
               let argTypes = [argType | (ProcArg _ _ argType _) <- args]
               if (length eTypes /= length argTypes) then
                 do
                   let errmsg = "procedure " ++ ident ++ " of arity " ++ 
                                (show (length eTypes)) ++ " does not exist"
                   logError errmsg pos
                   exitWith (ExitFailure 4)
               else
                 do
                   let zipTypes = zip eTypes argTypes
                   mapM_ cmpTypes zipTypes
                   return ()

        Nothing 
          -> do
               logError "undeclared procedure" pos
               exitWith (ExitFailure 4)
  where
    cmpTypes :: (BaseType, BaseType) -> IO ()
    cmpTypes (IntType, IntType) = return ()
    cmpTypes (IntType, FloatType) = return ()
    cmpTypes (FloatType, FloatType) = return ()
    cmpTypes (BoolType, BoolType) = return ()
    cmpTypes _ = do
                   logError "incorrect procedure parameter types" pos
                   exitWith (ExitFailure 4)

aStmt (table, pTable) (If pos expr stmts)
  = do
      eType <- aExpr pTable expr

      if eType /= BoolType then
        do
          logError "expression must be of type bool" pos
          exitWith (ExitFailure 4)

      else 
        do 
          mapM_ (aStmt (table, pTable)) stmts
          return ()

aStmt (table, pTable) (IfElse pos expr s1 s2)
  = do
      eType <- aExpr pTable expr
      
      if eType /= BoolType then
        do
          logError "expression must be of type bool" pos
          exitWith (ExitFailure 4)

      else
        do
          mapM_ (aStmt (table, pTable)) s1
          mapM_ (aStmt (table, pTable)) s2

          return ()

aStmt (table, pTable) (While pos expr stmts)
  = do
      eType <- aExpr pTable expr
    
      if eType /= BoolType then
        do
          logError "expression must be of type bool" pos
          exitWith (ExitFailure 4)

      else
        do
          mapM_ (aStmt (table, pTable)) stmts

          return ()
      
aProc :: GlobalSymTable -> Proc -> IO () 
aProc table (Proc pos ident args decls stmts)
  = do
      if ident == "main" && (length args) /= 0 then
        do
          logError "main procedure must have arity 0" pos
          exitWith (ExitFailure 4)
      else
        do
          let (ProcInfo _ pTable) = fromJust $ getProcInfo ident table
          mapM_ (aStmt (table, pTable)) stmts

          return ()

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
    
    (newSlot, procSymTable1) = insertArgs args 0 procSymTable

    procSymTable2 = insertVars decls newSlot procSymTable1

    table1 = updateProcSymTable ident args procSymTable2 table

analyse :: GoatProgram -> IO GlobalSymTable
analyse (GoatProgram procs)
  = do
      let startTable = initGlobalTable procs

          procInfo = getProcInfo "main" startTable

      case procInfo of
        Just (ProcInfo _ _) 
          -> do
               let globalTable = initAnalyse procs startTable
               mapM_ (aProc globalTable) procs
               return globalTable
        Nothing -> do
                     putStrLn "no procedure called main found"
                     exitWith (ExitFailure 4)
