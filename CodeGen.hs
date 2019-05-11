-- CodeGen.hs
-- Authors: Wen Tze Joshua Leong (wleong3)
--          Yiyue Wang (yiyue)

module CodeGen where

import System.Exit
import GoatAST


allocStackFrame :: Int -> IO ()
allocStackFrame n
  | n < 0     = exitWith (ExitFailure 1)
  | n == 0     = return ()
  | otherwise = do
                  putStr indent1
                  putStrLn ("push_stack_frame " ++ (show n))

deallocStackFrame :: Int -> IO ()
deallocStackFrame n
  | n < 0     = exitWith (ExitFailure 1)
  | n == 0     = return ()
  | otherwise = do
                  putStr indent1
                  putStrLn ("pop_stack_frame " ++ (show n)) 

genReg :: Int -> String
genReg rI
  = "r" ++ (show rI)

genExpr :: Expr -> Int -> IO ()
genExpr (StrConst s) rI
  = do 
      putStr indent1
      putStrLn ("string_const " ++ (genReg rI) ++ ", \"" ++ s ++ "\"")
       
  

genStmt :: Stmt -> IO ()
genStmt (Write expr)
  = case expr of
      StrConst str -> do
                        genExpr (StrConst str) 0
                        genCallBuiltin "print_string"
                        
      

-------------------------------------------------------------------------------
-- Generation of system based statements
-------------------------------------------------------------------------------
genReturn :: IO ()
genReturn 
  = putStrLn "return"

genHalt :: IO ()
genHalt 
  = do
      putStr indent1
      putStrLn "halt"

genCallBuiltin :: String -> IO ()
genCallBuiltin func
  = do
      putStr indent1
      putStr "call_builtin "
      putStrLn func
      


-- Perhaps a check for main arguments here? semantic analysis in code generation
-- Probably shoudl add in semantic analysis when parsing to get the line number
genFunc :: Func -> IO ()
genFunc (Func "main" args decls stmts)
  = do 
      putStrLn "proc_main:"
      allocStackFrame stackFrameSize
      mapM_ genStmt stmts     
      deallocStackFrame stackFrameSize
      genReturn 
  where
    stackFrameSize = length decls 
                     


genCode :: GoatProgram -> IO ()
genCode (GoatProgram [])
  = return ()
genCode (GoatProgram (f:funcs))
  = do
      genStart
      genFunc f

  where 
    genStart :: IO ()
    genStart
      = do
          putStr indent1
          putStrLn "call proc_main"
          genHalt

indent1 :: String
indent1 = take 4 (repeat ' ') 
