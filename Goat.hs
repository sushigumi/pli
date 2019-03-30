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
  = ["begin", "bool", "do", "else", "end", "false", "fi", "float", "if", "int",
     "od", "proc", "read", "ref", "then", "true", "val", "while", "write"]

goatOpnames
  = ["||", "&&", "!", "=", "!=", "<", "<=", ">", ">=", "+", "-", "*", "/"]

------------------------------------------------------------------------------
-- pProg is the topmost parsing function. It looks for all "proc" keywords and
-- parses all of them in a loop only keeps a lookout for "main"
------------------------------------------------------------------------------

pProg :: Parser [Func]
pProg 
  = do 
      funcs <- many pFunc
      return funcs

pFunc :: Parser Func
pFunc
  = do { reserved "proc"
       ; name <- identifier 
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

pBody :: Parser ([Decl], [Stmt])
pBody 
  = do
      decls <- many pDecl
      reserved "begin"
      stmts <- many pStmt
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
pStmt, pRead, pWrite, pCall, pAsg :: Parser Stmt

pStmt
  = choice [pIf, pWhile, pRead, pWrite, pCall, pAsg]

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
      ident <- identifier
      exprLst <- parens (sepBy pExpr comma)
      semi
      return (Call ident exprLst)

pIf
  = do
      reserved "if"
      exp <- pExpr
      reserved "then"
      ifStmts <- many pStmt
      elseside <- pElse
      reserved "fi"
      case elseside of
        Right elseStmts -> return (IfElse exp ifStmts elseStmts)
        Left _          -> return (If exp ifStmts)

pElse :: Parser (Either () [Stmt])
pElse
  = do
      reserved "else"
      stmts <- many pStmt
      return (Right stmts)
    <|>
    do 
      return (Left ())

pWhile 
  = do
      reserved "while"
      exp <- pExpr
      reserved "do"
      stmts <- many pStmt
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
       ; first <- natural
       ; arrayVal <- pSquare first
       ; char ']'
       ; case arrayVal of
           Left (first, sec) -> return (Array2d ident first sec)
           Right first       -> return (Array1d ident first)
       }
    <|>
    do { return (Elem ident) }
    <?> 
    "Invalid array declaration"

pSquare :: Integer -> Parser (Either (Integer, Integer) Integer)
pSquare first
  = do { comma 
       ; second <- natural
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
pExpr = buildExpressionParser table pTerm
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
  = pBool <|> pNum

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

pAfterDot :: Parser (Either () Int)
pAfterDot 
  = do 
      dot
      val <- natural
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

pLvalue :: Parser Lvalue
pLvalue
  = do
      ident <- identifier
      whiteSpace
      var <- pVar ident
      return (Lvalue var)
    <?>
    "lvalue"


main :: IO ()
main 
  = do { progname <- getProgName
       ; args <- getArgs
       ; checkArgs progname args
       ; input <- readFile (head args)
       ; let output = runParser pProg 0 "" input
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
