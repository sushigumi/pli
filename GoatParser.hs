-- GoatParser.hs
-- Authors: Wen Tze Joshua Leong (wleong3)
--          Yiyue Wang (yiyue)
-- This is the Parser for the programming language Goat.
-- mkAST produces an Abstract Syntax Tree for the code and if it is not 
-- syntactically well-formed, prints an error
-- This file uses some code obtained from GoatParser.hs provided by 
-- Harald Sondergaard in LMS for the COMP90045 subject

module GoatParser (mkAST) where

import GoatAST
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Pos
import qualified Text.Parsec.Token as Q

data MaybeOneOrTwo a
  = None
  | One a
  | Two a a
  | TooMany
  deriving (Eq, Show)

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
      funcs <- many1 pProc
      return (GoatProgram funcs)

-------------------------------------------------------------------------------
-- pProc parses functions and looks for the keyword 'proc' and parses 
-- the function name, its arguments, and then the function body
-------------------------------------------------------------------------------

pProc :: Parser Proc
pProc
  = do  
      pos <- getPosition
      reserved "proc"
      name <- identifier <?> "function name"
      args <- parens (pArgs)
      (decls, stmts) <- pBody
      return (Proc (comps pos) name args decls stmts)
       

-------------------------------------------------------------------------------
-- pArgs parses the arguments of the functions. It wants to recognize whether
-- there is a keyword of 'val' or 'ref' followed by a basetype and then an
-- identifier.
-------------------------------------------------------------------------------

pArgs :: Parser [ProcArg]
pArgs
  = do
      args <- sepBy pArg comma
      return args
      
pArg :: Parser ProcArg
pArg 
  = do  
      pos <- getPosition
      argMode <- pArgMode
      baseType <- pBaseType
      ident <- identifier
      return (ProcArg (comps pos) argMode baseType ident)
    <?>
    "function parameter starting with 'ref' or 'val'"
  where
    pArgMode :: Parser ArgMode
    pArgMode 
      = do
          reserved "val"
          return Val
        <|>
        do
          reserved "ref"
          return Ref
        <?>
        "'ref' or 'val' as a argument mode"

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
-- pManyIndices is a helper function to parse arrays or matrix which are part
-- of a variable declaration
-------------------------------------------------------------------------------

pDecl :: Parser Decl
pDecl
  = do
      pos <- getPosition
      baseType <- pBaseType
      ident <- identifier
      whiteSpace
      rest <- pMaybeIndices
      if rest == TooMany then
        unexpected "extra dimension(s)"
      else 
        do
          semi
          let
            typespec
              = case rest of 
                  None -> Base baseType
                  One n -> Array baseType n
                  Two m n -> Matrix baseType m n 
          return (Decl (comps pos) ident typespec)

pMaybeIndices :: Parser (MaybeOneOrTwo Int)
pMaybeIndices
  = do
      indices <- squares (sepBy1 natural comma)
      case indices of 
        [n] -> return (One (fromInteger n))
        [m,n] -> return (Two (fromInteger m) (fromInteger n))
        _ -> return TooMany
    <|>
    return None


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
      return (Read (comps pos) Nothing lvalue)

pWrite 
  = do
      pos <- getPosition
      reserved "write"
      expr <- pExpr
      semi
      return (Write (comps pos) Nothing expr)

pCall 
  = do
      pos <- getPosition
      reserved "call"
      ident <- identifier <?> "function identifier after call"
      exprLst <- parens (sepBy pExpr comma)
      semi
      return (Call (comps pos) Nothing ident exprLst)

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
        Right elseStmts 
          -> return (IfElse (comps pos) Nothing exp ifStmts elseStmts)
        Left _          
          -> return (If (comps pos) Nothing exp ifStmts)

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
      pos <- getPosition
      reserved "while"
      exp <- pExpr
      reserved "do"
      stmts <- many1 pStmt <?> "at least 1 statment"
      reserved "od"
      return (While (comps pos) Nothing exp stmts)

pAsg
  = do
      pos <- getPosition
      lvalue <- pLvalue
      whiteSpace
      reservedOp ":="
      whiteSpace
      rvalue <- pExpr
      semi
      return (Assign (comps pos) Nothing lvalue rvalue)

-------------------------------------------------------------------------------
-- pExpr is the main parser for expressions. It takes into account the operator
-- precedences and the fact that the binary operators are left-associative
-------------------------------------------------------------------------------
pExpr :: Parser Expr
pExpr = buildExpressionParser table pTerm <?> "expression"
  where 
    table = [ [prefix "-" UMinus]
            , [binary "*" OpMul, binary "/" OpDiv]
            , [binary "+" OpAdd, binary "-" OpSub]
            , [relation "=" OpEq, relation "!=" OpNe, relation "<" OpLt,
               relation "<=" OpLe, relation ">" OpGt, relation ">=" OpGe]
            , [prefix "!" Not]
            , [binLogic "&&" And]
            , [binLogic "||" Or] 
            ]
    prefix name fun
      = Prefix (do { pos <- getPosition; 
                     reservedOp name; 
                     return (fun (comps pos) Nothing) }
               )
    
    binary name op
      = Infix (do { pos <- getPosition;
                    reservedOp name; 
                    return (BinopExpr (comps pos) Nothing op) }
              ) AssocLeft

    relation name rel
      = Infix (do { pos <- getPosition; 
                    reservedOp name; 
                    return (RelExpr (comps pos) Nothing rel) }
              ) AssocNone

    binLogic name op
      = Infix (do { pos <- getPosition;
                    reservedOp name;
                    return (op (comps pos) Nothing) }
              ) AssocLeft

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
      return (StrConst (comps pos) Nothing str)
    <?>
    "constant"


pConst 
  = pBool <|> pNum <?> "constant"

pBool
  = do
      pos <- getPosition
      reserved "true"
      return (BoolConst (comps pos) Nothing True)
    <|>
    do
      pos <- getPosition
      reserved "false"
      return (BoolConst (comps pos) Nothing False)

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
          -> do
               let rep = read (first ++ val) :: Float
               return (FloatConst (comps pos) Nothing rep)
        Left _    
          -> do
               let rep = read first :: Int
               return (IntConst (comps pos) Nothing rep)

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
      exprs <- pMaybeIndexExprs
      if exprs == TooMany then
        unexpected "extra dimension(s)"
      else
        do
          case exprs of
            None -> return (Id (comps pos) Nothing ident)
            One e -> return (ArrayRef (comps pos) Nothing ident e)
            Two e1 e2 -> return (MatrixRef (comps pos) Nothing ident e1 e2)
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
      exprs <- pMaybeIndexExprs
      if exprs == TooMany then
        unexpected "extra dimension(s)"
      else
        do
          case exprs of
            None -> return (LId (comps pos) Nothing ident)
            One e -> return (LArrayRef (comps pos) Nothing ident e)
            Two e1 e2 -> return (LMatrixRef (comps pos) Nothing ident e1 e2)
    <?>
    "lvalue"

pMaybeIndexExprs :: Parser (MaybeOneOrTwo Expr)
pMaybeIndexExprs
  = do 
      exprs <- squares (sepBy1 pExpr comma)
      case exprs of
        [e] -> return (One e)
        [e1, e2] -> return (Two e1 e2)
        _ -> return TooMany
    <|>
    return None

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
