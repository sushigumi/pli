module PrettyGoat where

import GoatAST

-------------------------------------------------------------------------------
-- printFunc is a function used to print individual functions from the program
-------------------------------------------------------------------------------
printFunc :: Func -> IO ()
printFunc f
  = do
      putStrLn("asdf")


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
