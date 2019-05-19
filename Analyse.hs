module Analyse where

import SymTable
import qualified Data.Map.Strict as Map
import GoatAST

-- Analyse all the functions first to store all the parameters and names into
-- the symbol table before proceeding to each function's inside semantic 
-- analysis
aProcs :: [Proc] -> GlobalSymTable -> GlobalSymTable
aProcs [] table
  = table
aProcs ((Proc pos ident args decls stmts):procs) table
  = aProcs procs table1
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
  = aProcs procs startTable
  where
    startTable = initGlobalTable procs
