-- Goat.hs
-- Authors: Wen Tze Joshua Leong (wleong3)
--          Yiyue Wang (yiyue)
-- The compiler for the programming language Goat. 
-- To compile a program:
--  * Goat source_file 
--    where source_file is a Goat source file
-- To pretty print a program:
--  * Goat -p source_file
--    where source_file is a Goat source file
-- In this version 1, semantic errors are ignored when pretty-printing the Goat
-- source file. Only syntax and lexical errors are handled

module Main (main) where

import GoatParser
import Analyse
import CodePrint
import CodeGen
import System.Environment
import System.Exit

-------------------------------------------------------------------------------
-- This is the starting point for the Goat parser and parses the whole Goat 
-- program and returns either the AST of the program or 
-- if -p is specified, pretty prints the Goat program
-------------------------------------------------------------------------------
main :: IO ()
main 
  = do { progname <- getProgName
       ; args <- getArgs 
       ; (task, file) <- checkArgs progname args
       ; goat task file
       }

goat :: Task -> String -> IO ()
goat task file
  = do
      input <- readFile file
      let output = mkAST input
      case output of
        Right ast -> case task of
                       Compile -> do
                                    let symTable = analyse ast
                                    code <- genCode ast symTable
                                    printCode code
                                    exitWith ExitSuccess
-- CHANGEN PRETTY PRINT
                       Pretty  -> do
                                    print ast
                                    exitWith ExitSuccess
        Left err  -> do 
                       putStr "Parser error at "
                       print err
                       exitWith (ExitFailure 1)

---------------------------------------------------------------------------------
-- Handling command line arguments
-------------------------------------------------------------------------------
data Task
  = Pretty
  | Compile
  deriving (Show, Eq)

checkArgs :: String -> [String] -> IO (Task, String)
checkArgs progname args
  | length args == 0 = do
                         putStrLn ("Missing filename")
                         exitWith (ExitFailure 1)
  | length args == 1 = do
                         task <- parseTask ""
                         return (task, head args)
  | length args == 2 = do 
                         task <- parseTask $ head args
                         return (task, last args)
  | otherwise        = do
                         putStrLn ("Too many arguments")
                         putStrLn ("Usage: " ++ progname ++ "[-p] filename")
                         exitWith (ExitFailure 1)


parseTask :: String -> IO Task
parseTask option
  | option == "-p" = return Pretty
  | option == ""   = return Compile
  | otherwise      = do 
                        putStrLn ("Invalid options used")
                        exitWith (ExitFailure 1)
