module GoatIR where

import GoatAST

type Label = String

data Reg
  = Reg Int
  deriving (Eq)

instance Show Reg where
  show (Reg r) = "r" ++ (show r)

data Slot
  = Slot Int
  deriving (Eq)
  
instance Show Slot where
  show (Slot s) = show s

data BinopI
  = AddInt | AddReal | AddOff
  | SubInt | SubReal | SubOff
  | MulInt | MulReal
  | DivInt | DivReal
  | EqInt  | NeInt   | GtInt  | GeInt  | LtInt  | LeInt
  | EqReal | NeReal  | GtReal | GeReal | LtReal | LeReal
  | AndI   | OrI
  deriving (Show, Eq)

data UnopI
  = NegInt | NegReal 
  | NotI
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
  | IntConstI Reg Int
  | RealConstI Reg Float
  | StringConstI Reg String
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
  | LabelI Label
  deriving (Show, Eq)

data ProcCode
  = ProcCode Ident [Instr]
  deriving (Show, Eq)


