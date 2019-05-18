module Code where

type Label = String

data Reg
  = Reg Int

data Slot
  = Slot Int

data BinopI
  = AddInt | AddReal | AddOff
  | SubInt | SubReal | SubOff
  | MulInt | MulReal 
  | DivInt | DivReal
  | EqInt  | NeInt   | GtInt  | GeInt  | LtInt  | LeInt
  | EqReal | NeReal  | GtReal | GeReal | LtReal | LeReal

data UnopI
  = NegInt | NegReal
  | And | Or | Not

data Builtin
  = ReadInt  | ReadReal  | ReadBool
  | PrintInt | PrintReal | PrintBool | PrintString

data Instr
  = PushSF Int | PopSF Int
  | Store Slot Reg
  | Load Reg Slot
  | LoadAddr Reg Slot
  | LoadIndr Reg Reg
  | StoreIndr Reg Reg
  | IntConstI Reg Int
  | RealConstI Reg Float
  | StringConstI Reg String
  | BinInstr BinopI Reg Reg Reg
  | UnInstr UnopI Reg Reg
  | IntToReal Reg Reg
  | Move Reg Reg
  | BranchOnTrue Reg Label
  | BranchOnFalse Reg Label
  | BranchUncond Label
  | Call Label
  | CallBuiltin Builtin
  | Return
  | Halt
  | DebugReg Reg
  | DebugSlot Slot
  | DebugStack
