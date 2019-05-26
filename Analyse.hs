-- Analyse.hs
-- Authors: Wen Tze Joshua Leong (wleong3)
--          Yiyue Wang (yiyue)
-- This module contains all the functions needed for semantic analysis which 
-- does type checking and other checks before it is ready for code generation
-- This semantic analysis phase will take care of all the semantic analysis
-- so that a Goat Program which is free of any errors will be present for the 
-- code generator to generate intermediate representations

module Analyse (analyse) where

import SymTable
import qualified Data.Map.Strict as Map
import GoatAST
import GoatIR
import System.Exit
import Data.Maybe

-------------------------------------------------------------------------------
-- logError is a helper function to log errors based on the position given
-------------------------------------------------------------------------------
logError :: String -> Pos -> IO ()
logError str (line, col)
  = putStrLn $ str ++ " at line " ++ (show line) ++ " column " ++ (show col)

-------------------------------------------------------------------------------
-- checkArrayExprType and checkMatrixExprType checks the type of the expression
-- used as the accessor for arrays and matrix. 
-- If the expressions do not evaluate to an IntType then an error is thrown
-- and the program exits
-------------------------------------------------------------------------------
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

      if e1Type /= IntType || e2Type /= IntType then
        do
          logError "matrix accessor should be of type int" pos
          exitWith (ExitFailure 4)
      else
        return ()
 
-------------------------------------------------------------------------------
-- aLvalue does semantic analysis of lvalues and returns the base type of the
-- lvalue analysed
-------------------------------------------------------------------------------
aLvalue :: ProcSymTable -> Lvalue -> IO BaseType

-------------------------------------------------------------------------------
-- Checks single identifier
-- pos is the position of the identifier
-------------------------------------------------------------------------------
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

-------------------------------------------------------------------------------
-- Check array identifier
-------------------------------------------------------------------------------
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

-------------------------------------------------------------------------------
-- Check matrix identifier
-------------------------------------------------------------------------------
aLvalue pTable (LMatrixRef pos ident e1 e2)
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

-------------------------------------------------------------------------------
-- aExpr does semantic analysis and type checking for expressions
-- This checks everything that's not an L value
-------------------------------------------------------------------------------
aExpr :: ProcSymTable -> Expr -> IO BaseType
aExpr _ (BoolConst pos _)
  = return BoolType

aExpr _ (IntConst pos _)
  = return IntType

aExpr _ (FloatConst pos _)
  = return FloatType

aExpr _ (StrConst pos _)
  = return StringType

-------------------------------------------------------------------------------
-- Checks single identifier
-------------------------------------------------------------------------------
aExpr pTable (Id pos ident)
  = do
      let idInfo = getVarInfo ident pTable
      idType <- case idInfo of
                  Just (VarInfo bType _ _ _) -> return bType 
                  Nothing -> do
                               logError "undeclared variable" pos
                               exitWith (ExitFailure 4)

      return idType

-------------------------------------------------------------------------------
-- Checks array identifer
-------------------------------------------------------------------------------
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

-------------------------------------------------------------------------------
-- Checks matrix identifier
-------------------------------------------------------------------------------
aExpr pTable (MatrixRef pos ident e1 e2)
  = do
      e1Type <- aExpr pTable e1
      e2Type <- aExpr pTable e2

      if e1Type /= IntType || e2Type /= IntType then
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

-------------------------------------------------------------------------------
-- Checks && expression
-------------------------------------------------------------------------------
aExpr pTable (And pos e1 e2)
  = do
      e1Type <- aExpr pTable e1
      e2Type <- aExpr pTable e2
    
      if e1Type /= BoolType || e2Type /= BoolType then
        do
          logError "binary operator '&&' requires two bool operands" pos
          exitWith (ExitFailure 4)

      else 
        return BoolType

-------------------------------------------------------------------------------
-- Checks || expression
-------------------------------------------------------------------------------
aExpr pTable (Or pos e1 e2)
  = do
      e1Type <- aExpr pTable e1
      e2Type <- aExpr pTable e2
    
      if e1Type /= BoolType || e2Type /= BoolType then
        do
          logError "binary operator '||' requires two bool operands" pos
          exitWith (ExitFailure 4)
    
      else
        return BoolType

-------------------------------------------------------------------------------
-- Checks ! expression
-------------------------------------------------------------------------------
aExpr pTable (Not pos expr)
  = do
      eType <- aExpr pTable expr

      if eType /= BoolType then
        do 
          logError "unary operator '!' requires a bool operand" pos
          exitWith (ExitFailure 4)

      else
        return BoolType

-------------------------------------------------------------------------------
-- Checks comparison operand
-------------------------------------------------------------------------------
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

-------------------------------------------------------------------------------
-- Checks binary operand
-------------------------------------------------------------------------------
aExpr pTable (BinopExpr pos binop e1 e2)
  = do
      e1Type <- aExpr pTable e1
      e2Type <- aExpr pTable e2

      if (e1Type == BoolType || e2Type == BoolType) then
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

-------------------------------------------------------------------------------
-- Checks unary - operand
-------------------------------------------------------------------------------
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

-------------------------------------------------------------------------------
-- getLvalueType gets the type of the lvalue 
-------------------------------------------------------------------------------
getLvalueType :: Maybe VarInfo -> Pos-> IO BaseType
getLvalueType (Just (VarInfo bType _ _ _)) pos
  = return bType
getLvalueType Nothing pos
  = do
      logError "undeclared variable" pos
      exitWith (ExitFailure 4)

-------------------------------------------------------------------------------
-- aStmt does semantic analysis on a statement
-------------------------------------------------------------------------------
aStmt :: (GlobalSymTable, ProcSymTable) -> Stmt -> IO ()

-------------------------------------------------------------------------------
-- Checks assignment statement
-------------------------------------------------------------------------------
aStmt (table, pTable) (Assign pos lvalue expr) 
  = do
      let lvalueInfo = getVarInfo ident pTable

      aLvalue pTable lvalue
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

-------------------------------------------------------------------------------
-- Checks read statement
-------------------------------------------------------------------------------
aStmt (table, pTable) (Read pos lvalue)
  = do
      aLvalue pTable lvalue
      return ()

-------------------------------------------------------------------------------
-- Checks write statement
-------------------------------------------------------------------------------
aStmt (table, pTable) (Write pos expr) 
  = do
      eType <- aExpr pTable expr
      return ()

-------------------------------------------------------------------------------
-- Checks function call
-------------------------------------------------------------------------------
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

-------------------------------------------------------------------------------
-- Checks if statement
-------------------------------------------------------------------------------
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

-------------------------------------------------------------------------------
-- Checks if else statement
-------------------------------------------------------------------------------
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

-------------------------------------------------------------------------------
-- Checks while statement
-------------------------------------------------------------------------------
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

-------------------------------------------------------------------------------
-- aProc does semantic analysis on a procedure.
-- The main checks that it does is checking the main procedure and it must
-- have an arity of 0
-------------------------------------------------------------------------------
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

-------------------------------------------------------------------------------
-- initAnalyse starts analysis of the global procedures first to store all the
-- parameters and names into the global symbol so that global procedure calls
-- can be easily detected and verified
-- This step is done before proceeding to each procedure's inside semantic 
-- analysis
-------------------------------------------------------------------------------
initAnalyse :: [Proc] -> GlobalSymTable -> IO GlobalSymTable
initAnalyse [] table
  = return table
initAnalyse ((Proc pos ident args decls stmts):procs) table
  = case getProcInfo ident table of
      Just (ProcInfo _ procTable) 
        -> do 
             let (newSlot, procTable1) = insertArgs args 0 procTable
                 procTable2 = insertVars decls newSlot procTable1 
                 table1 = updateProcSymTable ident args procTable2 table
             initAnalyse procs table1
      Nothing 
        -> do
             logError "undeclared procedure" pos
             exitWith (ExitFailure 4)

-------------------------------------------------------------------------------
-- analyse starts the semantic analysis of the Goat program
-- Upon a semantic error of the Goat program, the analyser will print an error
-- message and exit with a status code of 4
--
-------------------------------------------------------------------------------
analyse :: GoatProgram -> IO GlobalSymTable
analyse (GoatProgram procs)
  = do
      let startTable = initGlobalTable procs

          procInfo = getProcInfo "main" startTable

      checkOneMain procs False

      case procInfo of
        Just (ProcInfo _ _) 
          -> do
               globalTable <- initAnalyse procs startTable
               mapM_ (aProc globalTable) procs
               return globalTable
        Nothing -> do
                     putStrLn "no procedure called main found"
                     exitWith (ExitFailure 4)
  where
    checkOneMain [] hasMain
      = if hasMain then 
          return ()
        else 
          do 
            logError "no main procedure found" (0,0)
            exitWith (ExitFailure 4)
    checkOneMain ((Proc pos ident _ _ _):ps) hasMain
      = if hasMain && ident == "main" then
          do 
            logError "more than one main found" pos
            exitWith (ExitFailure 4)
        else
          if ident == "main" then
            checkOneMain ps True
          else
            checkOneMain ps hasMain
