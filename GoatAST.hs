module GoatAST where

type Ident = String

data BaseType
  = BoolType | IntType | FloatType
  deriving (Show, Eq)


data Decl
  = Decl BaseType Ident
  deriving (Show, Eq)

data FuncArg
  = Val BaseType Ident
  | Ref BaseType Ident
  deriving (Show, Eq)

data Func
  = Func Ident [FuncArg] [Decl]
  deriving (Show, Eq)
