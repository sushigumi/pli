module CodeGen where

import GoatIR


genIntConst :: Reg -> Int -> [Instr]
genIntConst r val
  = [IntConst r val]

genRealConst :: Reg -> Float -> [Instr]
genRealConst r val
  = [RealConst r val]

genStrConst :: Reg -> String -> [Instr]
genStrConst r val 
  = [StringConst r val]









genWriteStmt :: [Instr]
genWriteStmt
  = [CallBuiltin PrintString]






-- Might need to change label 
genStart :: [Instr]
genStart 
  = [Call "proc_main", Halt]
