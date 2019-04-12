module PrettyGoat where
import GoatAST

-------------------------------------------------------------------------------
-- printIdent is a function used to print all the function and variable names
-------------------------------------------------------------------------------
printIdent :: Ident -> IO ()
printIdent ident = putStr ident


-------------------------------------------------------------------------------
-- Print list of arguments. Only add ", " if there are more than one arguments
-------------------------------------------------------------------------------
printArgs :: [FuncArg] -> IO ()
printArgs [] = return ()
printArgs [x] = printArg x
printArgs (arg:funcArgs) =
  do
    printArg arg
    putStr ", "
    printArgs funcArgs


-------------------------------------------------------------------------------
-- Print one argument
-------------------------------------------------------------------------------
printArg :: FuncArg -> IO ()
printArg (Val baseType ident) =
  do
    putStr "val "
    printBaseType baseType
    printIdent ident
printArg (Ref baseType ident) =
  do
    putStr "ref "
    printBaseType baseType
    printIdent ident


-------------------------------------------------------------------------------
-- Print base type
-------------------------------------------------------------------------------
printBaseType :: BaseType -> IO ()
printBaseType BoolType = putStr ("bool ")
printBaseType IntType = putStr ("int ")
printBaseType FloatType = putStr ("float ")


-------------------------------------------------------------------------------
-- Print list of declarations
-------------------------------------------------------------------------------
printDecls :: [Decl] -> IO ()
printDecls [] = return ()
printDecls (decl:decls) =
  do
    printDecl decl
    printDecls decls

-------------------------------------------------------------------------------
-- Print declaration
-------------------------------------------------------------------------------
printDecl :: Decl -> IO ()
printDecl (Decl baseType var) =
  do
    putStr (indent 1)
    printBaseType baseType
    printVar var
    putStrLn ";"


-------------------------------------------------------------------------------
-- Print variable. Add brackets around if it's an array
-------------------------------------------------------------------------------
printVar :: Var -> IO ()
printVar (Elem ident) = printIdent ident

printVar (Array1d ident expr) =
  do
    printIdent ident
    putStr "["
    printExpr expr
    putStr "]"

printVar (Array2d ident exprL exprR) =
  do
    printIdent ident
    putStr "["
    printExpr exprL
    putStr ", "
    printExpr exprR
    putStr "]"


-------------------------------------------------------------------------------
-- Print list of statements
-------------------------------------------------------------------------------
printStmts [] _ = return ()
printStmts (st:stmts) n =
  do
    printStmt st n
    printStmts stmts n


-------------------------------------------------------------------------------
-- Print one single statement
-------------------------------------------------------------------------------
printStmt :: Stmt -> Int -> IO ()

printStmt (Assign l expr) n =
  do
    putStr (indent n)
    printLvalue l
    putStr " := "
    printExpr expr
    putStrLn ";"

printStmt (Read l) n =
  do
    putStr (indent n ++ "read ")
    printLvalue l
    putStrLn ";"

printStmt (Write expr) n =
  do
    putStr (indent n ++ "write ")
    printExpr expr
    putStrLn ";"

printStmt (Call ident exprs) n =
  do
    putStr (indent n ++ "call ")
    printIdent ident
    putStr "("
    printExprs exprs
    putStrLn ");"

-- n is used to handle level of indentation
printStmt (If expr stmts) n =
  do
    putStr (indent n ++ "if ")
    printExpr expr
    putStrLn " then"
    printStmts stmts (n+1)
    putStrLn (indent n ++ "fi")

printStmt (IfElse expr stmtsIf stmtsElse) n =
  do
    putStr (indent n ++ "if ")
    printExpr expr
    putStrLn " then"
    printStmts stmtsIf (n+1)
    putStrLn (indent n ++ "else")
    printStmts stmtsElse (n+1)
    putStrLn (indent n ++ "fi")

printStmt (While expr stmts) n =
  do
    putStr (indent n ++ "while ")
    printExpr expr
    putStrLn " do"
    printStmts stmts (n+1)
    putStrLn (indent n ++ "od")


-------------------------------------------------------------------------------
-- Print list of expressions
-------------------------------------------------------------------------------
printExprs :: [Expr] -> IO ()
printExprs [] = return ()
printExprs [x] = printExpr x
printExprs (e:exprs) =
  do
    printExpr e
    putStr ", "
    printExprs exprs


-------------------------------------------------------------------------------
-- Print out Lvalue
-------------------------------------------------------------------------------
printLvalue :: Lvalue -> IO ()
printLvalue (Lvalue var) = printVar var


-------------------------------------------------------------------------------
-- Print expression
-------------------------------------------------------------------------------
printExpr :: Expr -> IO ()
printExpr (BoolConst b) = if b then putStr "true" else putStr "false"
printExpr (IntConst i) = putStr (show i)
printExpr (FloatConst f) = putStr (show f)
printExpr (StrConst s) = putStr ("\"" ++ s ++ "\"")
printExpr (Id var) = printVar var

-- Here we only add () around expression if expression is a binary operation
printExpr (BinopExpr binop exprL exprR) =
  do
    if isBinopExpr exprL then
      do
        putStr "("
        printExpr exprL
        putStr ")"
    else
      printExpr exprL
    printBinop binop
    if isBinopExpr exprR then
      do
        putStr "("
        printExpr exprR
        putStr ")"
    else
      printExpr exprR

printExpr (UnopExpr unop expr) =
  do
    printUnop unop
    printExpr expr


-------------------------------------------------------------------------------
-- print out binary operator
-------------------------------------------------------------------------------
printBinop :: Binop -> IO ()
printBinop Or = putStr " || "
printBinop And = putStr " && "
printBinop Equ = putStr " = "
printBinop NotEqu = putStr " != "
printBinop LThan = putStr " < "
printBinop ELThan = putStr " <= "
printBinop GThan = putStr " > "
printBinop EGThan = putStr " >= "
printBinop Add = putStr " + "
printBinop Sub = putStr " - "
printBinop Mul = putStr " * "
printBinop Div = putStr " / "


-------------------------------------------------------------------------------
-- print out unary operator
-------------------------------------------------------------------------------
printUnop :: Unop -> IO ()
printUnop UNot = putStr "!"
printUnop UMinus = putStr "-"


-------------------------------------------------------------------------------
-- printFunc is a function used to print individual functions from the program
-------------------------------------------------------------------------------
printFunc :: Func -> IO ()
printFunc (Func ident args decls stmts)
  = do
      putStr "proc "
      printIdent ident
      putStr " ("
      printArgs args
      putStrLn ")"
      printDecls decls
      putStrLn "begin"
      printStmts stmts 1
      putStrLn "end"


-------------------------------------------------------------------------------
-- prettyPrint is the top level function used to pretty print a Goat program
-- based on an AST (abstract syntax tree) generated for the code
-------------------------------------------------------------------------------
prettyPrint :: GoatProgram -> IO ()
prettyPrint (GoatProgram [])
  = return ()
prettyPrint (GoatProgram [f]) = printFunc f
prettyPrint (GoatProgram (f:funcs))
  = do
      printFunc f
      putStrLn ""
      prettyPrint (GoatProgram funcs)


-------------------------------------------------------------------------------
-- indent is a function used to return blank spaces for indentation
-- the parameter passed determines level of indentation. E.g. 1 means 4 spaces
-- 2 means 8 spaces, etc.
-------------------------------------------------------------------------------
indent :: Int -> String
indent n = take (n*4) (repeat ' ')

-------------------------------------------------------------------------------
-- isBinopExpr is a function used to determine whether an expr is binary op expr
-------------------------------------------------------------------------------
isBinopExpr :: Expr -> Bool
isBinopExpr (BinopExpr _ _ _) = True
isBinopExpr (BoolConst _) = False
isBinopExpr (IntConst _) = False
isBinopExpr (FloatConst _) = False
isBinopExpr (StrConst _) = False
isBinopExpr (Id _) = False
isBinopExpr (UnopExpr _ _) = False
