module GoatAST where

type Ident = String

data BaseType
  = BoolType | IntType | FloatType
  deriving (Show, Eq)

data Var
  = Elem Ident
  | Array1d Ident Integer
  | Array2d Ident Integer Integer
  deriving (Show, Eq)

data Lvalue
  = Lvalue Var
  deriving (Show, Eq)

data Decl
  = Decl BaseType Var 
  deriving (Show, Eq)

data Unop
  = UNot | UMinus
  deriving (Show, Eq)

data Binop 
  = Or | And 
  | Equ | NotEqu | LThan | ELThan | GThan | EGThan
  | Add | Sub | Mul | Div
  deriving (Show, Eq)

data Expr
  = BoolConst Bool
  | IntConst Int
  | FloatConst Float
  | StrConst String
  | Id Var
  | BinopExpr Binop Expr Expr
  | UnopExpr Unop Expr
  deriving (Show, Eq)

data Stmt
  = Assign Lvalue Expr
  | Read Lvalue
  | Write Expr
  | Call Ident [Expr]
  | If Expr [Stmt]
  | IfElse Expr [Stmt] [Stmt]
  | While Expr [Stmt]
  deriving (Show, Eq)

data FuncArg
  = Val BaseType Ident
  | Ref BaseType Ident
  deriving (Show, Eq)

data Func
  = Func Ident [FuncArg] [Decl] [Stmt]
  deriving (Show, Eq)
