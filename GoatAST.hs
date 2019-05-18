-- GoatAST.hs
-- Authors: Wen Tze Joshua Leong (wleong3)
--          Yiyue Wang (yiyue)
-- This file contains the data types which are used to create an Abstract
-- Syntax Tree for a Goat program.

module GoatAST where

type Ident = String

-- Position of the data type in the file (Column, Line number)
type Pos = (Int, Int)

-- Basetype
data BaseType
  = BoolType | IntType | FloatType
  deriving (Show, Eq)


-- TODO Change to Integer instead of Expr
-- Variables
data Var
  = Elem Pos Ident
  | Array1d Pos Ident Expr
  | Array2d Pos Ident Expr Expr
  deriving (Show, Eq)

-- Left value of assignment statements
data Lvalue
  = Lvalue Pos Var
  deriving (Show, Eq)

-- Function declarations
data Decl
  = Decl Pos BaseType Var 
  deriving (Show, Eq)

-- Unary operators
data Unop
  = UNot | UMinus
  deriving (Show, Eq)

-- Binary operators
data Binop 
  = Or | And 
  | Equ | NotEqu | LThan | ELThan | GThan | EGThan
  | Add | Sub | Mul | Div
  deriving (Show, Eq)

-- Expressions
data Expr
  = BoolConst Pos Bool
  | IntConst Pos Int
  | FloatConst Pos Float
  | StrConst Pos String
  | Id Pos Var
  | BinopExpr Pos Binop Expr Expr
  | UnopExpr Pos Unop Expr
  deriving (Show, Eq)

-- Statements
data Stmt
  = Assign Pos Lvalue Expr
  | Read Pos Lvalue
  | Write Pos Expr
  | Call Pos Ident [Expr]
  | If Pos Expr [Stmt]
  | IfElse Pos Expr [Stmt] [Stmt]
  | While Pos Expr [Stmt]
  deriving (Show, Eq)

-- Function arguments
data FuncArg
  = Val Pos BaseType Ident
  | Ref Pos BaseType Ident
  deriving (Show, Eq)

-- Function
data Func
  = Func Pos Ident [FuncArg] [Decl] [Stmt]
  deriving (Show, Eq)

data GoatProgram
  = GoatProgram [Func]
  deriving (Show, Eq)
