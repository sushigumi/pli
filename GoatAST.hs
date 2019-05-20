-- GoatAST.hs
-- Authors: Wen Tze Joshua Leong (wleong3)
--          Yiyue Wang (yiyue)
-- This file contains the data types which are used to create an Abstract
-- Syntax Tree for a Goat program.
-- Several modifications were made based on the GoatAST.hs provided by 
-- Harald Sondergaard found on the LMS 

module GoatAST where

type Ident = String

-- Position of the data type in the file (Column, Line number)
type Pos = (Int, Int)

-- Basetype
data BaseType
  = BoolType | IntType | FloatType | StringType
  deriving (Show, Eq)

data DeclType
  = Base BaseType
  | Array BaseType Int
  | Matrix BaseType Int Int
  deriving (Show, Eq)

data Lvalue
  = LId Pos Ident
  | LArrayRef Pos Ident Expr
  | LMatrixRef Pos Ident Expr Expr
  deriving (Show, Eq)

-- Procedure declarations
data Decl
  = Decl Pos Ident DeclType
  deriving (Show, Eq)

data Binop 
  = OpAdd | OpSub | OpMul | OpDiv
  deriving (Show, Eq)

data Relop 
  = OpEq | OpNe | OpGe | OpLe |OpGt | OpLt
  deriving (Show, Eq)

-- Expressions
data Expr
  = BoolConst Pos Bool
  | IntConst Pos Int
  | FloatConst Pos Float
  | StrConst Pos String
  | Id Pos Ident
  | ArrayRef Pos Ident Expr
  | MatrixRef Pos Ident Expr Expr
  | And Pos Expr Expr
  | Or Pos Expr Expr
  | Not Pos Expr
  | RelExpr Pos Relop Expr Expr
  | BinopExpr Pos Binop Expr Expr
  | UMinus Pos Expr
  deriving (Show, Eq)

-- Statements
data Stmt
  = Assign Pos Lvalue Expr 
  | Read Pos Lvalue 
  | Write Pos Expr 
  | ProcCall Pos Ident [Expr] 
  | If Pos Expr [Stmt]
  | IfElse Pos Expr [Stmt] [Stmt]
  | While Pos Expr [Stmt]
  deriving (Show, Eq)

data ArgMode 
  = Val | Ref
  deriving (Show, Eq)

-- Procedure arguments
data ProcArg
  = ProcArg Pos ArgMode BaseType Ident
  deriving (Show, Eq)

-- Procedure
data Proc
  = Proc Pos Ident [ProcArg] [Decl] [Stmt]
  deriving (Show, Eq)

data GoatProgram
  = GoatProgram [Proc]
  deriving (Show, Eq)


-------------------------------------------------------------------------------
-- Data structure for the annotated abstract syntax tree
-- New data structures were used instead of the existing ones because it would
-- be cumbersome to check for Maybe types since the attributes would only 
-- be present during the Semantic Analysis phase.
-- Having a new data structure which reflects the attributes instead of 
-- updating the previous tree will also allow a reduction in space used
-- since Haskell data structures are immutable and a copy is used for each
-- update.
-------------------------------------------------------------------------------
type Env = String

