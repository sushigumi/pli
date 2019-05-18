-- GoatParser.hs
-- Authors: Wen Tze Joshua Leong (wleong3)
--          Yiyue Wang (yiyue)
-- This is the Parser for the programming language Goat.
-- mkAST produces an Abstract Syntax Tree for the code and if it is not 
-- syntactically well-formed, prints an error

module GoatParser (mkAST) where

import GoatAST
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Q

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

-- Translate a source code position to a pair of integers (line, column)
comps :: SourcePos -> (Int, Int)
comps pos
  = (sourceLine pos, sourceColumn pos)

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
  = do  
      pos <- getPosition
      reserved "proc"
      name <- identifier <?> "function name"
      args <- parens (pArgs)
      (decls, stmts) <- pBody
      return (Func (comps pos) name args decls stmts)
       

-------------------------------------------------------------------------------
-- pArgs parses the arguments of the functions. It wants to recognize whether
-- there is a keyword of 'val' or 'ref' followed by a basetype and then an
-- identifier.
-------------------------------------------------------------------------------

pArgs :: Parser [FuncArg]
pArgs
  = do
      pos <- getPosition
      args <- sepBy pArg comma
      return args
      
pArg :: Parser FuncArg
pArg 
  = do  
      pos <- getPosition
      reserved "val"
      baseType <- pBaseType
      ident <- identifier
      return (Val (comps pos) baseType ident)
       
    <|>
    do  
      pos <- getPosition
      reserved "ref"
      baseType <- pBaseType
      ident <- identifier
      return (Ref (comps pos) baseType ident)
        
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
      pos <- getPosition
      baseType <- pBaseType
      ident <- identifier
      whiteSpace
      var <- pVar ident 
      semi
      return (Decl (comps pos) baseType var)

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
      pos <- getPosition
      reserved "read"
      lvalue <- pLvalue
      semi
      return (Read (comps pos) lvalue)

pWrite 
  = do
      pos <- getPosition
      reserved "write"
      expr <- pExpr
      semi
      return (Write (comps pos) expr)

pCall 
  = do
      pos <- getPosition
      reserved "call"
      ident <- identifier <?> "function identifier after call"
      exprLst <- parens (sepBy pExpr comma)
      semi
      return (Call (comps pos) ident exprLst)

pIf
  = do
      pos <- getPosition
      reserved "if"
      exp <- pExpr
      reserved "then"
      ifStmts <- many1 pStmt <?> "at least 1 statement"
      elseside <- pElse
      reserved "fi"
      case elseside of
        Right elseStmts -> return (IfElse (comps pos) exp ifStmts elseStmts)
        Left _          -> return (If (comps pos) exp ifStmts)

-- Helper for If to pass the else portion if it exists
pElse :: Parser (Either () [Stmt])
pElse
  = do
      pos <- getPosition
      reserved "else"
      stmts <- many1 pStmt <?> "at least 1 statement"
      return (Right stmts)
    <|>
    do 
      return (Left ())

pWhile 
  = do
      pos <- getPosition
      reserved "while"
      exp <- pExpr
      reserved "do"
      stmts <- many1 pStmt <?> "at least 1 statment"
      reserved "od"
      return (While (comps pos) exp stmts)

pAsg
  = do
      pos <- getPosition
      lvalue <- pLvalue
      whiteSpace
      reservedOp ":="
      whiteSpace
      rvalue <- pExpr
      semi
      return (Assign (comps pos) lvalue rvalue)

-------------------------------------------------------------------------------
-- pVar is the main parser for variables whether atomic or array variables
-------------------------------------------------------------------------------
pVar :: Ident -> Parser Var
pVar ident
  = do  
      pos <- getPosition
      char '['
      first <- pExpr <?> "size or initializer for variable with array type"
      arrayVal <- pSquare first
      char ']' <?> "']' to close array"
      whiteSpace
      case arrayVal of
        Left (first, sec) -> return (Array2d (comps pos) ident first sec)
        Right first       -> return (Array1d (comps pos) ident first)
       
    <|>
    do
      pos <- getPosition
      return (Elem (comps pos) ident) 
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
      = Prefix (do { pos <- getPosition; 
                     reservedOp name; 
                     return (UnopExpr (comps pos) fun) }
               )
    
    binary name op
      = Infix (do { pos <- getPosition;
                    reservedOp name; 
                    return (BinopExpr (comps pos) op) }
              ) AssocLeft

    relation name rel
      = Infix (do { pos <- getPosition; 
                    reservedOp name; 
                    return (BinopExpr (comps pos) rel) }
              ) AssocNone

    pTerm
      = choice [pIdent, pString, parens pExpr, pConst]

-------------------------------------------------------------------------------
-- pString, pConst, pBool and pNum parses a constant respectively
-------------------------------------------------------------------------------
pString, pConst, pBool, pNum, pIdent :: Parser Expr

-- pString parses a string but the string literal must be defined on a single
-- line and cannot consists of '\n'  or '\t' literaly in input but can include
-- these characters in the string itself
pString 
  = do
      pos <- getPosition
      char '"'
      str <- many (noneOf "\n\t\"")
      char '"'
      whiteSpace
      return (StrConst (comps pos) str)
    <?>
    "constant"


pConst 
  = pBool <|> pNum <?> "constant"

pBool
  = do
      pos <- getPosition
      reserved "true"
      return (BoolConst (comps pos) True)
    <|>
    do
      pos <- getPosition
      reserved "false"
      return (BoolConst (comps pos) False)

-- Parses a natural number or floating number
-- This parses the first digit as an integer and then the '.' and then the 
-- integer again if it is a float
pNum 
  = do
      pos <- getPosition
      first <- many1 digit
      afterDot <- pAfterDot
      case afterDot of
        Right val 
          -> return (FloatConst (comps pos) (read (first ++ val) :: Float))
        Left _    
          -> return (IntConst (comps pos) (read first :: Int))

-- Parses a natural number after the '.' if it is a float, else returns nothing
pAfterDot :: Parser (Either () String)
pAfterDot 
  = do 
      dot
      val <- many1 digit <?> "number after '.'"
      whiteSpace
      return (Right ('.':val))
    <|>
    do
      whiteSpace
      return (Left ()) 
   
-- Parses an identifier
pIdent 
  = do
      pos <- getPosition
      ident <- identifier
      var <- pVar ident
      return (Id (comps pos) var)
      <?>
      "identifier"
      

pBaseType :: Parser BaseType
pBaseType
  = do 
      reserved "bool" 
      return BoolType 
    <|>
    do  
      reserved "int" 
      return IntType 
    <|>
    do  
      reserved "float"
      return FloatType 
    <?>
    "type declaration"

pLvalue :: Parser Lvalue
pLvalue
  = do
      pos <- getPosition
      ident <- identifier
      whiteSpace
      var <- pVar ident
      return (Lvalue (comps pos) var)
    <?>
    "lvalue"

-------------------------------------------------------------------------------
-- Starting point for the Goat parser
-------------------------------------------------------------------------------
pMain :: Parser GoatProgram
pMain 
  = do
      whiteSpace
      p <- pProg
      eof
      return p

mkAST :: String -> Either ParseError GoatProgram
mkAST input
  = runParser pMain 0 "" input
