-- Goat.hs
-- The compiler for the programming language Goat. 
-- To compile a program:
--  * Goat source_file 
--    where source_file is a Goat source file
-- To pretty print a program:
--  * Goat -p source_file
--    where source_file is a Goat source file
-- In this version 1, semantic errors are ignored when pretty-printing the Goat
-- source file. Only syntax and lexical errors are handled.

import GoatAST
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Q
import System.Environment
import System.Exit

type Parser a
  = Parsec String Int a

lexer :: Q.TokenParser Int
-- MAY NEED TO EDIT OPLETTER BECAUSE OF WHAT IS USED IN THE TAIL???
-- CHECK IF CASE SENSITIVE
lexer
  = Q.makeTokenParser
    (emptyDef
    { Q.commentLine = "#"
    , Q.nestedComments = True
    , Q.identStart = letter
    , Q.opStart = oneOf "|&!=<>+-*/:"
    , Q.opLetter = oneOf "|&=+-*/"
    , Q.reservedNames = goatReserved
    , Q.reservedOpNames = goatOpnames
    })

whiteSpace = Q.whiteSpace lexer
lexeme = Q.lexeme lexer
natural = Q.natural lexer
identifier = Q.identifier lexer
colon = Q.colon lexer
semi = Q.semi lexer
comma = Q.comma lexer
parens = Q.parens lexer
squares = Q.squares lexer
reserved = Q.reserved lexer
reservedOp = Q.reservedOp lexer

goatReserved, goatOpnames :: [String]

goatReserved 
  = ["begin", "bool", "do", "else", "end", "false", "fi", "float", "if", "int",
     "od", "proc", "read", "ref", "then", "true", "val", "while", "write"]

goatOpnames
  = ["||", "&&", "!", "=", "!=", "<", "<=", ">", ">=", "+", "-", "*", "/"]

------------------------------------------------------------------------------
-- pProg is the topmost parsing function. It looks for all "proc" keywords and
-- parses all of them in a loop only keeps a lookout for "main"
------------------------------------------------------------------------------

pProgram :: Parser [Func]
pProgram 
  = do 
      funcs <- many pFunc
      return funcs

pFunc :: Parser Func
pFunc
  = do { reserved "proc"
       ; name <- identifier 
       ; args <- parens (pArgs)
       ; decl <- pBody
       ; return (Func name args decl)
       }

pArgs :: Parser [FuncArg]
pArgs
  = do 
      args <- sepBy pArg comma
      return args
      
pArg :: Parser FuncArg
pArg 
  = do { reserved "val"
       ; baseType <- pBaseType
       ; ident <- identifier
       ; return (Val baseType ident)
       }
    <|>
    do { reserved "ref"
       ; baseType <- pBaseType
       ; ident <- identifier
       ; return (Ref baseType ident)
       } 

pBody :: Parser [Decl]
pBody 
  = do
      decls <- many pDecl
      reserved "begin"
      reserved "end"
      return decls


pDecl :: Parser Decl
pDecl
  = do
      baseType <- pBaseType
      ident <- identifier
      whiteSpace
      semi
      return (Decl baseType ident)


pBaseType :: Parser BaseType
pBaseType
  = do { reserved "bool"; return BoolType }
    <|>
    do { reserved "int"; return IntType }
    <|>
    do { reserved "float"; return FloatType }



main :: IO ()
main 
  = do { progname <- getProgName
       ; args <- getArgs
       ; checkArgs progname args
       ; input <- readFile (head args)
       ; let output = runParser pProgram 0 "" input
       ; case output of 
           Right ast -> print ast
           Left  err -> do { putStr "Parse error at "
                           ; print err
                           }
       }

checkArgs :: String -> [String] -> IO ()
checkArgs _ [filename]
  = return ()
checkArgs progname _
  = do { putStrLn ("Usage: " ++ progname ++ " filename\n\n")
       ; exitWith (ExitFailure 1)
       }
