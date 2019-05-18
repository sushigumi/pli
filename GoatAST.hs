-- GoatAST.hs
-- Authors: Wen Tze Joshua Leong (wleong3)
--          Yiyue Wang (yiyue)
-- This file contains the data types which are used to create an Abstract
-- Syntax Tree for a Goat program.

module GoatAST where

type Ident = String

-- Basetype
data BaseType
  = BoolType | IntType | FloatType
  deriving (Show, Eq)

-- Variables
data Var
  = Elem Ident
  | Array1d Ident Expr
  | Array2d Ident Expr Expr
  deriving (Show, Eq)

-- Left value of assignment statements
data Lvalue
  = Lvalue Var
  deriving (Show, Eq)

-- Function declarations
data Decl
  = Decl BaseType Var 
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
  = BoolConst Bool
  | IntConst Int
  | FloatConst Float
  | StrConst String
  | Id Var
  | BinopExpr Binop Expr Expr
  | UnopExpr Unop Expr
  deriving (Show, Eq)

-- Statements
data Stmt
  = Assign Lvalue Expr
  | Read Lvalue
  | Write Expr
  | Call Ident [Expr]
  | If Expr [Stmt]
  | IfElse Expr [Stmt] [Stmt]
  | While Expr [Stmt]
  deriving (Show, Eq)

-- Function arguments
data FuncArg
  = Val BaseType Ident
  | Ref BaseType Ident
  deriving (Show, Eq)

-- Function
data Func
  = Func Ident [FuncArg] [Decl] [Stmt]
  deriving (Show, Eq)

data GoatProgram
  = GoatProgram [Func]
  deriving (Show, Eq)
