module GoatIR where

type Label = String

data Reg
  = Reg Int
  deriving (Show, Eq)

data Slot
  = Slot Int
  deriving (Show, Eq)

data BinopI
  = AddInt | AddReal | AddOff
  | SubInt | SubReal | SubOff
  | MulInt | MulReal
  | DivInt | DivReal
  | EqInt  | NeInt   | GtInt  | GeInt  | LtInt  | LeInt
  | EqReal | NeReal  | GtReal | GeReal | LtReal | LeReal
  | And | Or
  deriving (Show, Eq)

data UnopI
  = NegInt | NegReal 
  | Not
  deriving (Show, Eq)

data Builtin
  = ReadInt  | ReadReal  | ReadBool
  | PrintInt | PrintReal | PrintBool | PrintString
  deriving (Show, Eq)

data Instr
  = PushSF Int | PopSF Int
  | Store Slot Reg
  | Load Reg Slot
  | LoadAddr Reg Slot
  | LoadIndr Reg Reg
  | StoreIndr Reg Reg
  | IntConst Reg Int
  | RealConst Reg Float
  | StringConst Reg String
  | BinopInstr BinopI Reg Reg Reg
  | UnopInstr UnopI Reg Reg 
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
  deriving (Show, Eq)

