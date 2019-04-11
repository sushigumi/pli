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
import PrettyGoat
import System.FilePath
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Q
import System.Environment
import System.Exit
import System.Console.GetOpt

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

pFunc :: Parser Func
pFunc
  = do { reserved "proc"
       ; name <- identifier <?> "function name"
       ; args <- parens (pArgs)
       ; (decls, stmts) <- pBody
       ; return (Func name args decls stmts)
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
    <?>
    "function parameter starting with 'ref' or 'val'"

pBody :: Parser ([Decl], [Stmt])
pBody 
  = do
      decls <- many pDecl
      reserved "begin"
      stmts <- many1 pStmt <?> "at least 1 statement"
      reserved "end"
      return (decls, stmts)


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
-- call and assignment statements
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
       ; first <- natural <?> "size or initializer for variable with array type"
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

pSquare :: Integer -> Parser (Either (Integer, Integer) Integer)
pSquare first
  = do { comma 
       ; second <- natural <?> "']', size or initializer for array variable"
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


pString 
  = do
      char '"'
      str <- many (satisfy (/='"'))
      char '"'
      return (StrConst str)
    <?>
    "string"

pConst 
  = pBool <|> pNum <?> "boolean or number constant"

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

-- DO I NEED A ERR HERE?

pAfterDot :: Parser (Either () Int)
pAfterDot 
  = do 
      dot
      val <- natural <?> "number after '.'"
      return (Right (fromInteger val :: Int))
    <|>
    do
      return (Left ()) 
   

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
       ; (args, files) <- getArgs >>= parseArgs
       ; goat args files
       }

goat :: [Flag] -> [String] -> IO ()
goat args files
  = do 
      input <- readFile (head files)
      let output = runParser pMain 0 "" input
      case output of 
        Right ast -> if Pretty `elem` args
                       then prettyPrint ast
                       else print ast
        Left  err -> do { putStr "Parser error at "
                        ; print err
                        }

-------------------------------------------------------------------------------
-- Command line arguments handling
-------------------------------------------------------------------------------
data Flag
  = Pretty
  | Help
  deriving (Show, Eq)

flags 
  = [Option ['p'] []            (NoArg Pretty)
        "Pretty prints the program and outputs it to stdout"
    ,Option []    ["help"]      (NoArg Help)
        "Print this help message"
    ]

-- Checks the file arguments to ensure that only one file is passed in as 
-- an argument
checkFileArg :: [String] -> IO ()
checkFileArg files
  | flen < 1 = do 
                 putStrLn ("Missing filename in arguments")
                 exitWith (ExitFailure 1)
  | flen > 1 = do 
                 putStrLn ("Too many files in arguments")
                 exitWith (ExitFailure 1) 
  | otherwise = case takeExtension (head files) of
                  ".gt" -> return ()
                  _ -> do
                         putStrLn ("Invalid file")
                         exitWith (ExitFailure 1)

  where 
    flen = length files

-- This function parses the command line arguments into more understandable 
-- structure.
-- Splits the options and files into two separate variables and returns them
-- If there exists an error, the program terminates and prints an error message
parseArgs argv 
  = case getOpt Permute flags argv of
      (args, fs, []) -> do
         let files = if null fs
                       then []
                       else fs

         -- If there is not exactly one file then print an error message
         checkFileArg files
         if Help `elem` args
            then do putStrLn (usageInfo header flags)
                    exitWith ExitSuccess
            else return (args, files)

      (_, _, errs) -> do
         putStrLn (concat errs ++ usageInfo header flags)
         exitWith (ExitFailure 1)
  where
    header = "Usage: Goat [-p] file"
