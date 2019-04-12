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
module Main where

import GoatAST
import PrettyGoat
import Text.Parsec
import Text.Parsec.Expr
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
dot = Q.dot lexer
semi = Q.semi lexer
comma = Q.comma lexer
parens = Q.parens lexer
squares = Q.squares lexer
reserved = Q.reserved lexer
reservedOp = Q.reservedOp lexer

goatReserved, goatOpnames :: [String]

goatReserved 
  = ["begin", "bool", "call", "do", "else", "end", "false", "fi", "float", 
     "if", "int", "od", "proc", "read", "ref", "then", "true", "val", "while", 
     "write"]

goatOpnames
  = ["||", "&&", "!", "=", "!=", "<", "<=", ">", ">=", "+", "-", "*", "/"]

------------------------------------------------------------------------------
-- pProg is the topmost parsing function. It looks for all "proc" keywords and
-- parses all of them in a loop only keeps a lookout for "main"
------------------------------------------------------------------------------

pProg :: Parser GoatProgram
pProg 
  = do 
      funcs <- many1 pFunc
      return (GoatProgram funcs)

-------------------------------------------------------------------------------
-- pFunc parses functions and looks for the keyword 'proc' and parses 
-- the function name, its arguments, and then the function body
-------------------------------------------------------------------------------

pFunc :: Parser Func
pFunc
  = do { reserved "proc"
       ; name <- identifier <?> "function name"
       ; args <- parens (pArgs)
       ; (decls, stmts) <- pBody
       ; return (Func name args decls stmts)
       }

-------------------------------------------------------------------------------
-- pArgs parses the arguments of the functions. It wants to recognize whether
-- there is a keyword of 'val' or 'ref' followed by a basetype and then an
-- identifier.
-------------------------------------------------------------------------------

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
    <?>
    "function parameter starting with 'ref' or 'val'"

-------------------------------------------------------------------------------
-- pBody parses the function body. It looks for a sequence of declarations 
-- followed by a sequence of statements. The statements are enclosed by 
-- keywords 'begin' and 'end'
-------------------------------------------------------------------------------

pBody :: Parser ([Decl], [Stmt])
pBody 
  = do
      decls <- many pDecl
      reserved "begin"
      stmts <- many1 pStmt <?> "at least 1 statement"
      reserved "end"
      return (decls, stmts)

-------------------------------------------------------------------------------
-- pDecl looks for a sequence of one or more declarations
-------------------------------------------------------------------------------

pDecl :: Parser Decl
pDecl
  = do
      baseType <- pBaseType
      ident <- identifier
      whiteSpace
      var <- pVar ident 
      semi
      return (Decl baseType var)

-------------------------------------------------------------------------------
-- pStmt is the main parser for statements. It wants to recognise read, write
-- call, if, if else, while and assignment statements
-------------------------------------------------------------------------------

pStmt, pRead, pWrite, pCall, pIf, pWhile, pAsg :: Parser Stmt

pStmt
  = choice [pIf, pWhile, pRead, pWrite, pCall, pAsg]
    <?>
    "statement"

pRead 
  = do
      reserved "read"
      lvalue <- pLvalue
      semi
      return (Read lvalue)

pWrite 
  = do
      reserved "write"
      expr <- pExpr
      semi
      return (Write expr)

pCall 
  = do
      reserved "call"
      ident <- identifier <?> "function identifier after call"
      exprLst <- parens (sepBy pExpr comma)
      semi
      return (Call ident exprLst)

pIf
  = do
      reserved "if"
      exp <- pExpr
      reserved "then"
      ifStmts <- many1 pStmt <?> "at least 1 statement"
      elseside <- pElse
      reserved "fi"
      case elseside of
        Right elseStmts -> return (IfElse exp ifStmts elseStmts)
        Left _          -> return (If exp ifStmts)

-- Helper for If to pass the else portion if it exists
pElse :: Parser (Either () [Stmt])
pElse
  = do
      reserved "else"
      stmts <- many1 pStmt <?> "at least 1 statement"
      return (Right stmts)
    <|>
    do 
      return (Left ())

pWhile 
  = do
      reserved "while"
      exp <- pExpr
      reserved "do"
      stmts <- many1 pStmt <?> "at least 1 statment"
      reserved "od"
      return (While exp stmts)

pAsg
  = do
      lvalue <- pLvalue
      whiteSpace
      reservedOp ":="
      whiteSpace
      rvalue <- pExpr
      semi
      return (Assign lvalue rvalue)

-------------------------------------------------------------------------------
-- pVar is the main parser for variables whether atomic or array variables
-------------------------------------------------------------------------------
pVar :: Ident -> Parser Var
pVar ident
  = do { char '['
       ; first <- pExpr <?> "size or initializer for variable with array type"
       ; arrayVal <- pSquare first
       ; char ']' <?> "']' to close array"
       ; case arrayVal of
           Left (first, sec) -> return (Array2d ident first sec)
           Right first       -> return (Array1d ident first)
       }
    <|>
    do { return (Elem ident) }
    <?>
    "variable"

-- Parses the second half of an array, which is after the comma.
-- If there is no comma, then just return the first number, else returns
-- both numbers
pSquare :: Expr -> Parser (Either (Expr, Expr) Expr)
pSquare first
  = do { comma 
       ; second <- pExpr <?> "']', size or initializer for array variable"
       ; return (Left (first, second)) 
       }
    <|>
    do { return (Right first) }

-------------------------------------------------------------------------------
-- pExpr is the main parser for expressions. It takes into account the operator
-- precedences and the fact that the binary operators are left-associative
--
-- Based on the unambiguous CFG: 
--
-------------------------------------------------------------------------------
pExpr :: Parser Expr
pExpr = buildExpressionParser table pTerm <?> "expression"
  where 
    table = [ [prefix "-" UMinus]
            , [binary "*" Mul, binary "/" Div]
            , [binary "+" Add, binary "-" Sub]
            , [relation "=" Equ, relation "!=" NotEqu, relation "<" LThan,
               relation "<=" ELThan, relation ">" GThan, relation ">=" EGThan]
            , [prefix "!" UNot]
            , [binary "&&" And]
            , [binary "||" Or] 
            ]
    prefix name fun
      = Prefix (do { reservedOp name; return (UnopExpr fun) })
    
    binary name op
      = Infix (do { reservedOp name; return (BinopExpr op) }) AssocLeft

    relation name rel
      = Infix (do { reservedOp name; return (BinopExpr rel) }) AssocNone

    pTerm
      = choice [pString, parens pExpr, pConst, pIdent]

-------------------------------------------------------------------------------
-- pString, pConst, pBool and pNum parses a constant respectively
-------------------------------------------------------------------------------
pString, pConst, pBool, pNum, pIdent :: Parser Expr

-- pString parses a string but the string literal must be defined on a single
-- line and cannot consists of '\n'  or '\t' literaly in input but can include
-- these characters in the string itself
pString 
  = do
      char '"'
      str <- many (noneOf "\n\t\"")
      char '"'
      return (StrConst str)
    <?>
    "constant"


pConst 
  = pBool <|> pNum <?> "constant"

pBool
  = do
      reserved "true"
      return (BoolConst True)
    <|>
    do
      reserved "false"
      return (BoolConst False)

-- Parses a natural number or floating number
-- This parses the first digit as an integer and then the '.' and then the 
-- integer again if it is a float
pNum 
  = do
      first <- natural
      afterDot <- pAfterDot
      case afterDot of
        Right val -> return (FloatConst (read (show first ++ "." ++ show val) :: Float))
        Left _    -> return (IntConst (fromInteger first :: Int))

-- Parses a natural number after the '.' if it is a float, else returns nothing
pAfterDot :: Parser (Either () Int)
pAfterDot 
  = do 
      dot
      val <- natural <?> "number after '.'"
      return (Right (fromInteger val :: Int))
    <|>
    do
      return (Left ()) 
   
-- Parses an identifier
pIdent 
  = do
      ident <- identifier
      var <- pVar ident
      return (Id var)
      <?>
      "identifier"
      

pBaseType :: Parser BaseType
pBaseType
  = do { reserved "bool"; return BoolType }
    <|>
    do { reserved "int"; return IntType }
    <|>
    do { reserved "float"; return FloatType }
    <?>
    "type declaration"

pLvalue :: Parser Lvalue
pLvalue
  = do
      ident <- identifier
      whiteSpace
      var <- pVar ident
      return (Lvalue var)
    <?>
    "lvalue"

-------------------------------------------------------------------------------
-- This is the starting point for the Goat parser and parses the whole Goat 
-- program and returns either the AST of the program or 
-- if -p is specified, pretty prints the Goat program
-------------------------------------------------------------------------------
pMain :: Parser GoatProgram
pMain 
  = do
      whiteSpace
      p <- pProg
      eof
      return p

main :: IO ()
main 
  = do { progname <- getProgName
       ; args <- getArgs 
       ; (task, file) <- checkArgs progname args
       ; goat task file
       }

goat :: Task -> String -> IO ()
goat task file
  | task == Compile = do
                        putStrLn ("Sorry, cannot generate code yet")
                        exitWith ExitSuccess
  | task == Pretty = do
                       input <- readFile file
                       let output = runParser pMain 0 "" input
                       case output of 
                         Right ast -> do
                                        prettyPrint ast
                                        exitWith ExitSuccess
                         Left  err -> do { putStr "Parser error at "
                                         ; print err
                                         ; exitWith (ExitFailure 1)
                                         }

-------------------------------------------------------------------------------
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
