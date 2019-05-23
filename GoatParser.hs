---------------------------------------------------------
-- Programming Language Implementation COMP90045 Project1
-- Implemented by Shjie Liu, Wenqing Xue, Minjian Chen
---------------------------------------------------------

module GoatParser (ast) where

import GoatFormat
import GoatAST
import Data.Char
import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Q
import System.Environment
import System.Exit
import GHC.Float
import Data.Text

type Parser a
  = Parsec String () a

lexer :: Q.TokenParser ()
lexer
  = Q.makeTokenParser
    (emptyDef
    { Q.commentLine     = "#"
    , Q.nestedComments  = True
    , Q.identStart      = letter
    , Q.opStart         = oneOf "+-*:|&=!<>"
    , Q.opLetter        = oneOf "+-*:|&=!<>"
    , Q.reservedNames   = myReserved
    , Q.reservedOpNames = myOpnames
    })

whiteSpace = Q.whiteSpace lexer
lexeme     = Q.lexeme lexer
natural    = Q.natural lexer
float      = Q.float lexer
identifier = Q.identifier lexer
colon      = Q.colon lexer
semi       = Q.semi lexer
comma      = Q.comma lexer
parens     = Q.parens lexer
squares    = Q.squares lexer
reserved   = Q.reserved lexer
reservedOp = Q.reservedOp lexer
brackets   = Q.brackets lexer

myReserved, myOpnames :: [String]

-- Reserved tokens
myReserved
  = [ "begin",
      "bool",
      "do",
      "call",
      "else",
      "end",
      "false",
      "fi",
      "float",
      "if",
      "int",
      "od",
      "proc",
      "read",
      "ref",
      "then",
      "true",
      "val",
      "while",
      "write"]

-- Reserved operators
myOpnames
  = [ "+", "-", "*", "/", ":=", "||", "&&",
      "=", "!=", "<", "<=", ">", ">=", "!" ]

-- Parser for Goat program
pProg :: Parser GoatProg
pProg
  = do
      -- Parse one or more procedures
      procs <- many1 pProc
      return (Prog procs)

-- Parser for procedure
pProc :: Parser Proc
pProc
  = do
      -- Get source position
      pos <- getPosition
      -- Reserved token "proc"
      reserved "proc"
      -- Parse procedure identifier
      ident <- identifier
      -- Parse parameters separated by comma "," within parentheses
      params <- parens (pParam `sepBy` comma)
      -- Parse procedure body
      (decls, stmts) <- pProcBody
      return (Proc pos ident params decls stmts)

-- Parser for parameter
pParam :: Parser Param
pParam
  = do
      -- Get source position
      pos <- getPosition
      -- Parse indicator: "val", "ref"
      indic <- pIndicator
      -- Parse base type: "bool", "int", "float"
      btype <- pBaseType
      -- Parse parameter identifier
      ident <- identifier
      return (Param pos indic btype ident)

-- Parser for parameter indicator
pIndicator :: Parser Indicator
pIndicator
  = lexeme (
      do
        -- Reserved token "val"
        reserved "val"
        return Val
      <|>
      do
        -- Reserved token "ref"
        reserved "ref"
        return Ref
    )

-- Parser for procedure body: declarations and statements
pProcBody :: Parser ([Decl], [Stmt])
pProcBody
  = do
      -- Procedure consists of zero or more declarations
      decls <- many pDecl
      -- Reserved token "begin"
      reserved "begin"
      -- Procedure consists of one or more statements
      stmts <- many1 pStmt
      -- Reserved token "end"
      reserved "end"
      return (decls, stmts)

-- Parser for declarations
pDecl :: Parser Decl
pDecl
  = do
      -- Get source position
      pos <- getPosition
      -- Parse base type
      btype <- pBaseType
      -- Parse Variables
      dvar <- pDeclVar
      -- Declaration ends with semicolon ";"
      semi
      return (Decl pos btype dvar)

-- Parser for variables in declarations
pDeclVar:: Parser DeclVar
pDeclVar
  -- Try to parse variable with shape
  = try (
      do
        -- Parse identifier
        ident <- identifier
        -- Parse shape
        shape <- pShape
        return (ShapeVar ident shape)
    )
    <|>
    -- Otherwise, parse variable as base type
    do
      -- Parse identifier
      ident <- identifier
      return (DBaseVar ident)

-- Parser for variables in statements
pStmtVar :: Parser StmtVar
pStmtVar
  -- Try to parse variable with index
  = try (
      do
        -- Parse identifier
        ident <- identifier
        -- Parse index
        index <- pIndex
        return (IndexVar ident index)
    )
    <|>
    -- Otherwise, parse variable as base type
    do
      -- Parse identifier
      ident <- identifier
      return (SBaseVar ident)

-- Parser for shape
pShape :: Parser Shape
pShape
  -- Shape is surrounded by brackets
  = brackets (
      -- Try to recognise shape as a matrix
      try (
        do
          -- Parse the first integer
          n1 <- natural
          -- Shape is separated by comma ","
          lexeme (char ',')
          -- Parse the second integer
          n2 <- natural
          return (SMatrix (fromInteger n1 :: Int) (fromInteger n2 :: Int))
      )
      <|>
      -- Otherwise, recognise shape as an array
      do
        -- Parse one integer
        n <- natural
        return (SArray (fromInteger n :: Int))
    )

-- Parser for index
pIndex :: Parser Index
pIndex
  -- Index is surrounded by brackets
  = brackets (
      -- Try to recognise index as a matrix
      try (
        do
          -- Parse the first expression
          e1 <- pExpr
          -- Index is separated by comma ","
          lexeme (char ',')
          -- Parse the second expression
          e2 <- pExpr
          return (IMatrix e1 e2)
      )
      <|>
      -- Otherwise, recognise index as an array
      do
        -- Parse one expression
        e <- pExpr
        return (IArray e)
    )

-- Parser for basetype: bool, int, float
pBaseType :: Parser BaseType
pBaseType
  = do
      -- Reserved token "bool"
      reserved "bool"
      return BoolType
    <|>
    do
      -- Reserved token "int"
      reserved "int"
      return IntType
    <|>
    do
      -- Reserved token "float"
      reserved "float"
      return FloatType

-----------------------------------------------------------------
-- pStmt is the main parser for statements. It wants to recognise
-- any type of statements as specified in rules.
-----------------------------------------------------------------

pStmt, pAssign, pRead, pWrite, pSWrite, pCall, pIf, pWhile :: Parser Stmt

-- Parser for all statements, it recognises all statements and chooses one
pStmt
  = choice [pAssign, pRead, pWrite, pSWrite, pCall, pIf, pWhile]

-- Parser for assign statement
pAssign
  = do
      -- Get source position
      pos <- getPosition
      -- Recognise the left value
      lvalue <- pStmtVar
      -- Reserved operator ":="
      reservedOp ":="
      -- Parse the assigned expression
      rvalue <- pExpr
      -- Assign statement ends with semicolon ";"
      semi
      return (Assign pos lvalue rvalue)

-- Parser for read statement
pRead
  = do
      -- Get source position
      pos <- getPosition
      -- Reserved token "read"
      reserved "read"
      -- Recognise the variable
      var <- pStmtVar
      -- Read statement ends with semicolon ";"
      semi
      return (Read pos var)

-- Parser for write expression statement
pWrite
  = try (
      do
        -- Get source position
        pos <- getPosition
        -- Reserved token "write"
        reserved "write"
        -- Recognise the expression
        expr <- pExpr
        -- Write statement ends with semicolon ";"
        semi
        return (Write pos expr)
    )

-- Parser for write string statement
pSWrite
  = do
      -- Get source position
      pos <- getPosition
      -- Reserved token "write"
      reserved "write"
      -- Recognise the string
      str <- pString
      -- Write statement ends with semicolon ";"
      semi
      return (SWrite pos str)

-- Parser for call statement
pCall
  = do
      -- Get source position
      pos <- getPosition
      -- Reserved token "call"
      reserved "call"
      -- Parse the identifier
      ident <- identifier
      -- Parse expression separated by comma "," within parentheses
      exprs <- parens (pExpr `sepBy` comma)
      -- Call statement ends with semicolon ";"
      semi
      return (Call pos ident exprs)

-- Parser for If and If Else statements
pIf
  = do
      -- Get source position
      pos <- getPosition
      -- Parse the common part "if <expr> then <stmt-list>"
      -- Reserved token "if"
      reserved "if"
      -- Parse the expression
      expr <- pExpr
      -- Reserved token "then"
      reserved "then"
      -- Parse the statements
      thsms <- many1 pStmt
      -- Parse the else part
      elsms <-
        -- If token "else" followed, parse "else <stmt-list> fi"
        do
          -- Reserved token "else"
          reserved "else"
          -- Parse one or more statements
          e <- many1 pStmt
          -- Reserved token "fi"
          reserved "fi"
          return e
        <|>
        -- If token "fi" followed, return empty list
        do
          -- Reserved token "fi"
          reserved "fi"
          return []
      return (If pos expr thsms elsms)

-- Parser for while statements "while <expr> do <stmt-list> od"
pWhile
  = do
      -- Get source position
      pos <- getPosition
      -- Reserved token "while"
      reserved "while"
      -- Parse the expression
      expr <- pExpr
      -- Reserved token "do"
      reserved "do"
      -- Parse one or more statements
      stmts <- many1 pStmt
      -- Reserved token "od"
      reserved "od"
      return (While pos expr stmts)

-- Parser for string
pString :: Parser String
pString
  = lexeme (
      do
        -- String is surrounded by two quotes
        char '"'
        -- Parse characters except newline / tab characters and quotes
        str <- many (satisfy (not . (`elem` "\"\n\t")))
        char '"'
        return (str)
    )

------------------------------------------------------------------
-- pExpr is the main parser for expressions. It wants to recognise
-- any type of expression as specified in rules.
------------------------------------------------------------------

pExpr, pOrExpr, pAndExpr, pNegExpr :: Parser Expr
pComExpr, pTerm, pFactor, pBaseExpr :: Parser Expr

-- Parser for expressions, the 1st level is connected by "||"
pExpr
  = chainl1 pOrExpr pOrOp

-- Parser for Or expressions, the 2nd level is connected by "&&"
pOrExpr
  = chainl1 pAndExpr pAndOp

-- Parser for And expressions, the 3rd level is connected by '!'
pAndExpr
  = try (
      do
        -- Get source position
        pos <- getPosition
        -- Reserved operator "!"
        reservedOp "!"
        expr <- pNegExpr
        return (Neg pos expr)
    )
    <|>
    do
      pNegExpr

-- Parser for Neg expressions, the 4th level is connected by Compare operators
pNegExpr
  = try (
      do
        c1 <- pComExpr
        co <- pComOp
        c2 <- pComExpr
        return (co c1 c2)
    )
    <|>
    do
      pComExpr
-- Parser for Com expressions, the 5th level is connected by Term operators
pComExpr
  = chainl1 pTerm pTermOp

-- Parser for Term expressions, the 6th level is connected by Factor operators
pTerm
  = chainl1 pFactor pFactorOp

-- Parser for Factor expressions, the 7th level is connected by UMinus
pFactor
  = try (
      do
        -- Get source position
        pos <- getPosition
        -- Reserved operator "-"
        reservedOp "-"
        expr <- pFactor
        return (UMinus pos expr)
    )
    <|>
    do
      pBaseExpr

-- Parser for Base expressions,
-- the 8th level is (expr), statement variable or constant
pBaseExpr
  = choice [parens pExpr,
            pIdent,
            pConst]

pConst, pBool, pNum, pIdent :: Parser Expr

-- Parser for int, float or bool constant
pConst
  = choice [pBool,
            pNum]

-- Parser for bool constant
pBool
  = do
      -- Get source position
      pos <- getPosition
      -- Reserved token "true"
      reserved "true"
      return (BoolConst pos True)
    <|>
    do
      -- Get source position
      pos <- getPosition
      -- Reserved token "false"
      reserved "false"
      return (BoolConst pos False)

-- Parser for int or float constant
pNum
  -- Try to parse the number into float type first
  = try (
      do
        -- Get source position
        pos <- getPosition
        n <- float
        return (FloatConst pos (double2Float n :: Float))
    )
    <|>
    -- Otherwise parse into int type
    do
      -- Get source position
      pos <- getPosition
      n <- natural
      return (IntConst pos (fromInteger n :: Int))

-- Parser for Id in expression
pIdent
  = do
      -- Get source position
      pos <- getPosition
      svar <- pStmtVar
      return (Id pos svar)

pOrOp, pAndOp, pComOp, pTermOp, pFactorOp :: Parser (Expr -> Expr -> Expr)

-- Parser for Or operator
pOrOp
  = do
      -- Get source position
      pos <- getPosition
      -- Reserved operator "||"
      reservedOp "||"
      return (Or pos)

-- Parser for And operator
pAndOp
  = do
      -- Get source position
      pos <- getPosition
      -- Reserved operator "&&"
      reservedOp "&&"
      return (And pos)

-- Parser for Compare operators
pComOp
  = choice [pEqualOp,
            pNotEqualOp,
            pLessOp,
            pLessEqualOp,
            pGreaterOp,
            pGreaterEqualOp]

-- Parser for Term operators
pTermOp
  = choice [pAddOp,
            pMinusOp]

-- Parser for Factor operators
pFactorOp
  = choice [pMulOp,
            pDivOp]

pEqualOp, pNotEqualOp :: Parser (Expr -> Expr -> Expr)
pLessOp, pLessEqualOp :: Parser (Expr -> Expr -> Expr)
pGreaterOp, pGreaterEqualOp :: Parser (Expr -> Expr -> Expr)

-- Parser for Equal operators
pEqualOp
  = do
      -- Get source position
      pos <- getPosition
      -- Reserved operator "="
      reservedOp "="
      return (Equal pos)

-- Parser for Not Equal operators
pNotEqualOp
  = do
      -- Get source position
      pos <- getPosition
      -- Reserved operator "!="
      reservedOp "!="
      return (NotEqual pos)

-- Parser for Less operators
pLessOp
  = do
      -- Get source position
      pos <- getPosition
      -- Reserved operator "<"
      reservedOp "<"
      return (Less pos)

-- Parser for Less Equal operators
pLessEqualOp
  = do
      -- Get source position
      pos <- getPosition
      -- Reserved operator "<="
      reservedOp "<="
      return (LessEqual pos)

-- Parser for Greater operators
pGreaterOp
  = do
      -- Get source position
      pos <- getPosition
      -- Reserved operator ">"
      reservedOp ">"
      return (Greater pos)

-- Parser for Greater Equal operators
pGreaterEqualOp
  = do
      -- Get source position
      pos <- getPosition
      -- Reserved operator ">="
      reservedOp ">="
      return (GreaterEqual pos)

pAddOp, pMinusOp, pMulOp, pDivOp :: Parser (Expr -> Expr -> Expr)

-- Parser for Add operators
pAddOp
  = do
      -- Get source position
      pos <- getPosition
      -- Reserved operator "+"
      reservedOp "+"
      return (Add pos)

-- Parser for Minus operators
pMinusOp
  = do
      -- Get source position
      pos <- getPosition
      -- Reserved operator "-"
      reservedOp "-"
      return (Minus pos)

-- Parser for Mul operators
pMulOp
  = do
      -- Get source position
      pos <- getPosition
      -- Reserved operator "*"
      reservedOp "*"
      return (Mul pos)

-- Parser for Div operators
pDivOp
  = do
      -- Get source position
      pos <- getPosition
      -- Reserved operator "/"
      reservedOp "/"
      return (Div pos)

-- Main Parser
pMain :: Parser GoatProg
pMain
  = do
      -- Remove whitespaces in front of the program
      whiteSpace
      -- Parse the Goat program
      p <- pProg
      -- End of File
      eof
      return p

ast :: String -> Either ParseError GoatProg
ast input
  = runParser pMain () "" input
