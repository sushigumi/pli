module SymTable where

import qualified Data.Map.Strict as Map
import GoatAST
import GoatIR

-- Probably need to store the enclosing scope inside
-- Maybe a Nothignt o represent whether it s a val or ref or nothing
data VarInfo
  = VarInfo BaseType ArgMode Slot
  deriving (Show, Eq)

-- STORE ENCLOSING SCOPE INSIDE
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
insertVar (Decl _ ident decltype) s table
  = case decltype of
      Base baseType 
        -> (s+1, Map.insert ident (VarInfo baseType Val (Slot s)) table)
      Array baseType n 
        -> (s+n, Map.insert ident (VarInfo baseType Val (Slot s)) table)
      Matrix baseType m n 
        -> (s+m*n, Map.insert ident (VarInfo baseType Val (Slot s)) table)

insertVars :: [Decl] -> Int -> ProcSymTable -> ProcSymTable
insertVars [] _ table
  = table
insertVars (d:decls) slot table
  = insertVars decls newSlot newTable
  where 
    (newSlot, newTable) = insertVar d slot table

insertArg :: ProcArg -> Int -> ProcSymTable -> (Int, ProcSymTable)
insertArg (ProcArg _ mode baseType ident) s table
  = (s+1, Map.insert ident (VarInfo baseType mode (Slot s)) table)

insertArgs :: [ProcArg] -> Int -> ProcSymTable -> (Int, ProcSymTable)
insertArgs [] slot table
  = (slot, table)
insertArgs (a:args) slot table
  = insertArgs args newSlot newTable
  where
    (newSlot, newTable) = insertArg a slot table


updateProcSymTable :: String -> [ProcArg] -> ProcSymTable -> GlobalSymTable
                   -> GlobalSymTable
updateProcSymTable ident args procTable globalTable
  = Map.insert ident (ProcInfo args procTable) globalTable


getProcInfo :: String -> GlobalSymTable -> Maybe ProcInfo
getProcInfo ident globalTable
  = Map.lookup ident globalTable

getVarInfo :: String -> ProcSymTable -> Maybe VarInfo
getVarInfo ident procTable
  = Map.lookup ident procTable


getSize :: Map.Map k a -> Int
getSize table
  = Map.size table 
