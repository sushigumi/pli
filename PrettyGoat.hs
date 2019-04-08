-- TODO: Need to handle indentation. Ignore temporarily

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
    putStr "    "
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
    indentation <- printStmt st n
    printStmts stmts indentation

printStmt :: Stmt -> IO ()
printStmt (Assign l expr) n =
  do
    printLvalue l n
    return n

printLvalue :: Lvalue -> IO ()

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
