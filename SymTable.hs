module SymTable where

import qualified Data.Map.Strict as Map
import GoatAST
import GoatIR

-------------------------------------------------------------------------------
-- Symbol Table variables
-- ProcInfo represents a procedure info
-- VarInfo represents variable info
-- and 
-- AddVarInfo represents additional variable info which is mostly used for
-- arrays and matrices 
-------------------------------------------------------------------------------
data AddVarInfo
  = IdInfo
  | ArrayInfo Int
  | MatrixInfo Int Int
  deriving (Show, Eq)

data VarInfo
  = VarInfo BaseType ArgMode Slot AddVarInfo
  deriving (Show, Eq)

-- STORE ENCLOSING SCOPE INSIDE
data ProcInfo
  = ProcInfo [ProcArg] ProcSymTable
  deriving (Show, Eq)

type ProcSymTable 
  = Map.Map String VarInfo

type GlobalSymTable 
  = Map.Map String ProcInfo 

-------------------------------------------------------------------------------
-- initGlobalTable initialises the Global Symbol Table with empty procedure 
-- information but creates an instance of the procedure information
-------------------------------------------------------------------------------
initGlobalTable :: [Proc] -> GlobalSymTable
initGlobalTable procs
  = Map.fromList (map makePair procs)
  where
    makePair :: Proc -> (String, ProcInfo)
    makePair (Proc _ ident args _ _)
      = (ident, (ProcInfo args Map.empty))


-------------------------------------------------------------------------------
-- insertVar inserts VarInfo into the specified Procedure Symbol Table
-------------------------------------------------------------------------------
insertVar :: Decl -> Int -> ProcSymTable -> (Int, ProcSymTable)
insertVar (Decl _ ident decltype) s table
  = case decltype of
      Base baseType 
        -> (s+1, Map.insert ident (VarInfo baseType Val (Slot s) IdInfo) table)
      Array baseType n 
        -> (s+n, Map.insert ident (VarInfo baseType Val (Slot s) (ArrayInfo n))
            table)
      Matrix baseType m n 
        -> (s+m*n, Map.insert ident (VarInfo baseType Val (Slot s)
            (MatrixInfo m n)) table)

-------------------------------------------------------------------------------
-- insertVars inserts all variables into the Procedure Symbol Table
-------------------------------------------------------------------------------
insertVars :: [Decl] -> Int -> ProcSymTable -> ProcSymTable
insertVars [] _ table
  = table
insertVars (d:decls) slot table
  = insertVars decls newSlot newTable
  where 
    (newSlot, newTable) = insertVar d slot table

-------------------------------------------------------------------------------
-- insertArg inserts an argument information for procedure into the Procedure
-- Symbol Table
-------------------------------------------------------------------------------
insertArg :: ProcArg -> Int -> ProcSymTable -> (Int, ProcSymTable)
insertArg (ProcArg _ mode baseType ident) s table
  = (s+1, Map.insert ident (VarInfo baseType mode (Slot s) IdInfo) table)

-------------------------------------------------------------------------------
-- insertArgs inserts all the arguments into the Procedure Symbol Table
-------------------------------------------------------------------------------
insertArgs :: [ProcArg] -> Int -> ProcSymTable -> (Int, ProcSymTable)
insertArgs [] slot table
  = (slot, table)
insertArgs (a:args) slot table
  = insertArgs args newSlot newTable
  where
    (newSlot, newTable) = insertArg a slot table

-------------------------------------------------------------------------------
-- updateProcSymTable updates a procedure symbol table
-------------------------------------------------------------------------------
updateProcSymTable :: String -> [ProcArg] -> ProcSymTable -> GlobalSymTable
                   -> GlobalSymTable
updateProcSymTable ident args procTable globalTable
  = Map.insert ident (ProcInfo args procTable) globalTable

-------------------------------------------------------------------------------
-- getProcInfo gets the procedure information from the global symbol table
-------------------------------------------------------------------------------
getProcInfo :: String -> GlobalSymTable -> Maybe ProcInfo
getProcInfo ident globalTable
  = Map.lookup ident globalTable

-------------------------------------------------------------------------------
-- getVarInfo gets the variable information from the procedure symbol table
-------------------------------------------------------------------------------
getVarInfo :: String -> ProcSymTable -> Maybe VarInfo
getVarInfo ident procTable
  = Map.lookup ident procTable

-------------------------------------------------------------------------------
-- getStackFrameSize gets the stack frame size needed for each procedure
-- The stack frame size is the number of slots used for all the declarations
-- and arguments in the procedure
-------------------------------------------------------------------------------
getStackFrameSize :: ProcSymTable -> Int
getStackFrameSize table 
  = countSize procInfos
  where
    procInfos = Map.elems table

    countSize [] = 0
    countSize (v:vs) 
      = case info of
          IdInfo -> 1 + (countSize vs)
          ArrayInfo n -> n + (countSize vs)
          MatrixInfo m n -> m * n + (countSize vs)
      where
        (VarInfo _ _ _ info) = v

