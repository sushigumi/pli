-- SymTable.hs
-- Authors: Wen Tze Joshua Leong (wleong3)
--          Yiyue Wang (yiyue)
-- Module which provides symbol table services to the compiler
-- for Goat

module SymTable where

import Data.Map.Strict

data Symtable
  = Symtable 
    { upTable :: Symtable
    , downTable :: [Symtable]
    , hasStackFrame :: Bool
    , members :: Map String Int -- STC
    }
