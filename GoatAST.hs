-- GoatAST.hs
-- Authors: Wen Tze Joshua Leong (wleong3)
--          Yiyue Wang (yiyue)
-- This file contains the data types which are used to create an Abstract
-- Syntax Tree for a Goat program.
-- Several modifications were made based on the GoatAST.hs provided by 
-- Harald Sondergaard found on the LMS 

module GoatAST where

-- Attributes for a node in the AST
data Attr
  = Attr { env :: String
         , baseType :: BaseType
         }
  deriving (Show, Eq)

type Ident = String

-- Position of the data type in the file (Column, Line number)
type Pos = (Int, Int)

-- Basetype
data BaseType
  = BoolType | IntType | FloatType 
  deriving (Show, Eq)

data DeclType
  = Base BaseType
  | Array BaseType Int
  | Matrix BaseType Int Int
  deriving (Show, Eq)

data Lvalue
  = LId Pos (Maybe Attr) Ident
  | LArrayRef Pos (Maybe Attr) Ident Expr
  | LMatrixRef Pos (Maybe Attr) Ident Expr Expr
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
  = BoolConst Pos (Maybe Attr) Bool
  | IntConst Pos (Maybe Attr) Int
  | FloatConst Pos (Maybe Attr) Float
  | StrConst Pos (Maybe Attr) String
  | Id Pos (Maybe Attr) Ident
  | ArrayRef Pos (Maybe Attr) Ident Expr
  | MatrixRef Pos (Maybe Attr) Ident Expr Expr
  | And Pos (Maybe Attr) Expr Expr
  | Or Pos (Maybe Attr) Expr Expr
  | Not Pos (Maybe Attr) Expr
  | RelExpr Pos (Maybe Attr) Relop Expr Expr
  | BinopExpr Pos (Maybe Attr) Binop Expr Expr
  | UMinus Pos (Maybe Attr) Expr
  deriving (Show, Eq)

-- Statements
data Stmt
  = Assign Pos (Maybe Attr) Lvalue Expr 
  | Read Pos (Maybe Attr) Lvalue 
  | Write Pos (Maybe Attr) Expr 
  | Call Pos (Maybe Attr) Ident [Expr] 
  | If Pos (Maybe Attr) Expr [Stmt]
  | IfElse Pos (Maybe Attr) Expr [Stmt] [Stmt]
  | While Pos (Maybe Attr) Expr [Stmt]
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

