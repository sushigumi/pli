-- TODO: Need to handle indentation. Ignore temporarily
-- TODO: Remove last empty line

module PrettyGoat where
import GoatAST

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
      putStrLn ""

-------------------------------------------------------------------------------
-- printIdent is a function used to print all the function and variable names
-------------------------------------------------------------------------------
printIdent :: Ident -> IO ()
printIdent ident = putStr ident

printArgs :: [FuncArg] -> IO ()
printArgs [] = return ()
printArgs [x] = printArg x
printArgs (arg:funcArgs) =
  do
    printArg arg
    putStr ", "
    printArgs funcArgs

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

printBaseType :: BaseType -> IO ()
printBaseType BoolType = putStr ("bool ")
printBaseType IntType = putStr ("int ")
printBaseType FloatType = putStr ("float ")

printDecls :: [Decl] -> IO ()
printDecls [] = return ()
printDecls (decl:decls) =
  do
    printDecl decl
    printDecls decls

printDecl :: Decl -> IO ()
printDecl (Decl baseType var) =
  do
    putStr (indent 1)
    printBaseType baseType
    printVar var
    putStrLn ";"

printVar :: Var -> IO ()
printVar (Elem ident) = printIdent ident
printVar (Array1d ident i) =
  do
    printIdent ident
    putStr ("[" ++ show i ++ "]")
printVar (Array2d ident i j) =
  do
    printIdent ident
    putStr ("[" ++ show i ++ ", " ++ show j ++ "]")

printStmts [] _ = return ()
printStmts (st:stmts) n =
  do
    printStmt st n
    printStmts stmts n


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


printExprs :: [Expr] -> IO ()
printExprs [] = return ()
printExprs [x] = printExpr x
printExprs (e:exprs) =
  do
    printExpr e
    putStr ", "
    printExprs exprs

printLvalue :: Lvalue -> IO ()
printLvalue (Lvalue var) = printVar var

printExpr :: Expr -> IO ()
printExpr expr = putStr "Expr"
-------------------------------------------------------------------------------
-- indent is a function used to return blank spaces for indentation
-- the parameter passed determines level of indentation. E.g. 1 means 4 spaces
-- 2 means 8 spaces, etc.
-------------------------------------------------------------------------------
indent :: Int -> String
indent n = take (n*4) (repeat ' ')

-------------------------------------------------------------------------------
-- prettyPrint is the top level function used to pretty print a Goat program
-- based on an AST (abstract syntax tree) generated for the code
-------------------------------------------------------------------------------
prettyPrint :: GoatProgram -> IO ()
prettyPrint (GoatProgram []) 
  = return ()
prettyPrint (GoatProgram (f:funcs))
  = do
      printFunc f
      prettyPrint (GoatProgram funcs)
