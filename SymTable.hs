module SymTable where

import qualified Data.Map.Strict as Map
import GoatAST
import GoatIR

data VarInfo
  = VarInfo BaseType Slot
  deriving (Show, Eq)

data ProcInfo
  = ProcInfo [ProcArg] ProcSymTable
  deriving (Show, Eq)

type ProcSymTable 
  = Map.Map String VarInfo

type GlobalSymTable 
  = Map.Map String ProcInfo 


initGlobalTable :: [Proc] -> GlobalSymTable
initGlobalTable procs
  = Map.fromList (map makePair procs)
  where
    makePair :: Proc -> (String, ProcInfo)
    makePair (Proc _ ident args _ _)
      = (ident, (ProcInfo args Map.empty))


-- Declarations, the counter for slot, procsymtable
insertVar :: Decl -> Int -> ProcSymTable -> (Int, ProcSymTable)
insertVar (Decl _ ident decltype) slot table
  = case decltype of
      Base baseType 
        -> (slot+1, Map.insert ident (VarInfo baseType (Slot slot)) table)
      Array baseType n 
        -> (slot+n, Map.insert ident (VarInfo baseType (Slot slot)) table)
      Matrix baseType m n 
        -> (slot+m*n, Map.insert ident (VarInfo baseType (Slot slot)) table)

insertVars :: [Decl] -> Int -> ProcSymTable -> ProcSymTable
insertVars [] _ table
  = table
insertVars (d:decls) slot table
  = insertVars decls newSlot newTable
  where 
    (newSlot, newTable) = insertVar d slot table


updateProcSymTable :: String -> [ProcArg] -> ProcSymTable -> GlobalSymTable
                   -> GlobalSymTable
updateProcSymTable ident args procTable globalTable
  = Map.insert ident (ProcInfo args procTable) globalTable


getProcInfo :: String -> GlobalSymTable -> Maybe ProcInfo
getProcInfo ident globalTable
  = Map.lookup ident globalTable
