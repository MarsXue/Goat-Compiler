---------------------------------------------------------
-- Programming Language Implementation COMP90045 Project1
-- Implemented by Shjie Liu, Wenqing Xue, Minjian Chen
---------------------------------------------------------
module Main where

import GoatFormat
import GoatAST
import Data.Char
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Q
import System.Environment
import System.Exit
import GHC.Float
import Data.Text

type Parser a
  = Parsec String () a

data Task
  = Compile | Pprint | Parse
  deriving (Show, Eq)

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
-- Reserved Tokens
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
-- Reserved Operation characters
myOpnames
  = [ "+", "-", "*", "/", ":=", "||", "&&",
      "=", "!=", "<", "<=", ">", ">=", "!" ]

-- Parser for Goat Program
pProg :: Parser GoatProg
pProg
  = do
      procs <- many1 pProc -- Goat Program contains one or more procdures
      return (Prog procs)

-- Parser for Procdures
pProc :: Parser Proc
pProc
  = do
      reserved "proc" -- Reserved token "proc"
      ident <- identifier -- parse Procdure identifier
      params <- parens (pParam `sepBy` comma) -- parse parameters within ()
      (decls, stmts) <- pProcBody -- parse procdure body
      return (Proc ident params decls stmts)

-- Parser for one parameter
pParam :: Parser Param
pParam
  = do
      ind <- pIndicator -- Parse indicator "val" or "ref"
      t <- pBaseType -- Parse basetype
      ident <- identifier -- Parse parameter identifier
      return (Param ind t ident)

-- Parser for parameter indicator
pIndicator :: Parser Indicator
pIndicator
  = lexeme (
      do
        reserved "val"
        return Val
      <|>
      do
        reserved "ref"
        return Ref
    )

-- Parser for procdure body
pProcBody :: Parser ([Decl], [Stmt])
pProcBody
  = do
      decls <- many pDecl -- Procdure consists of zero or more declarations
      reserved "begin" -- Reserved token
      stmts <- many1 pStmt -- Procdure consists of one or more statements
      reserved "end" -- Reserved token
      return (decls, stmts)

-- Parser for declarations
pDecl :: Parser Decl
pDecl
  = do
      btype <- pBaseType -- Parse base type
      dvar <- pDeclVar -- Parse Variables
      semi -- End token ';'
      return (Decl btype dvar)

-- Parser for variables in declarations
pDeclVar:: Parser DeclVar
pDeclVar
    -- Try to parse the variable with shape.
  = try (
      do
        ident <- identifier -- Parse identifier
        shape <- pShape -- Parse shape
        return (ShapeVar ident shape)
    )
    <|>
    -- If fail, parse the variable as basetype.
    do
      ident <- identifier -- Parse identifier
      return (DBaseVar ident)

-- Parser for variables in statements
pStmtVar :: Parser StmtVar
pStmtVar
    -- Try to parse the variable with index.
  = try (
      do
        ident <- identifier -- parse identifier
        index <- pIndex -- parse index
        return (IndexVar ident index)
    )
    <|>
    -- If fail, parse the variable as BaseType.
    do
      ident <- identifier
      return (SBaseVar ident)

-- Parser for shape
pShape :: Parser Shape
pShape
    -- Shape is surrounded by '[' and ']'.
  = brackets (
      -- Try to recognise shape as a matrix.
      try (
        do
          n1 <- natural -- Parse the first integer.
          lexeme (char ',')
          n2 <- natural -- Parse the second integer.
          return (DShape (fromInteger n1 :: Int) (fromInteger n2 :: Int))
      )
      <|>
      -- If fail, recognise shape as an array.
      do
        n <- natural -- Parse the integer.
        return (SShape (fromInteger n :: Int))
    )

-- Parser for index
pIndex :: Parser Index
pIndex
    -- Index is surrounded by '[' and ']'.
  = brackets (
      -- Try to recognise index as a matrix.
      try (
        do
          e1 <- pExpr -- Parse the first expression.
          lexeme (char ',')
          e2 <- pExpr -- Parse the second expression.
          return (DIndex e1 e2)
      )
      <|>
      -- If fail, recognise index as an array.
      do
        e <- pExpr -- Parse the expression.
        return (SIndex e)
    )

-- Parser for basetype: int, float, bool
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

-- -----------------------------------------------------------------
-- --  pStmt is the main parser for statements. It wants to recognise
-- --  any type of statements as specified in rules.
-- -----------------------------------------------------------------
pStmt, pAssign, pRead, pWrite, pSWrite, pCall, pIf, pWhile :: Parser Stmt

-- Parser for all statements, it recognise all statements and make choice.
pStmt
  = choice [pAssign, pRead, pWrite, pSWrite, pCall, pIf, pWhile]

-- Parser for assignment statements.
pAssign
  = do
      lvalue <- pStmtVar -- Recognise the left value
      reservedOp ":=" -- Reserved token
      rvalue <- pExpr -- Parse the assigned expression
      semi
      return (Assign lvalue rvalue)

-- Parser for read statements.
pRead
  = do
      reserved "read" -- Reserved token
      var <- pStmtVar -- Recognise the variable
      semi
      return (Read var)

-- Parser for write expression statements.
pWrite
  = try (
      do
        reserved "write" -- Reserved token
        expr <- pExpr -- Recognise the expression
        semi
        return (Write expr)
    )

-- Parser for write string statements.
pSWrite
  = do
      reserved "write" -- Reserved token
      str <- pString -- Recognise the string
      semi
      return (SWrite str)

-- Parser for call statements.
pCall
  = do
      reserved "call" -- Reserved token
      ident <- identifier -- Parse the identifier
      exprs <- parens (pExpr `sepBy` comma) -- Parse the comma separated
      semi                                  -- expression list surrounded by
      return (Call ident exprs)             -- '(' and ')'.

-- Parser for If and If Else statements
pIf
  = do
      -- Parse the common part "if <expr> then <stmt-list>"
      reserved "if"
      expr <- pExpr
      reserved "then"
      thsms <- many1 pStmt
      -- Parse the else part
      elsms <-
        do
          -- if exist token "else", parse else part.
          reserved "else"
          e <- many1 pStmt
          reserved "fi"
          return e
        <|>
        -- if token "fi" followed, return empty list.
        do
          reserved "fi"
          return []
      return (If expr thsms elsms)

-- Parser for while statements
pWhile
  = do
      reserved "while"
      expr <- pExpr -- Parse expression
      reserved "do"
      stmts <- many1 pStmt -- Parse one or more statements
      reserved "od"
      return (While expr stmts)

-- Parser for string
pString :: Parser String
pString
  = lexeme (
      do
        -- String is surrounded by two '"' characters
        char '"'
        -- Parse characters except newline/tab characters and '"'
        str <- many (satisfy (not . (`elem` "\"\n\t")))
        char '"'
        return (str)
    )

-- -----------------------------------------------------------------
-- --  pExpr is the main parser for expressions. It wants to recognise
-- --  any type of expression as specified in rules.
-- -----------------------------------------------------------------
pExpr, pOrExpr, pAndExpr, pNegExpr :: Parser Expr
pComExpr, pTerm, pFactor, pBaseExpr :: Parser Expr

-- Parser for expressions, the first level is connected by "||"
pExpr
  = chainl1 pOrExpr pOrOp

-- Parser for Or expressions, the second level is connected by "&&"
pOrExpr
  = chainl1 pAndExpr pAndOp

-- Parser for And expressions, the third level is connected by '!'
pAndExpr
  = try (
      do
        reservedOp "!"
        expr <- pNegExpr
        return (Neg expr)
    )
    <|>
    do
      pNegExpr

-- Parser for Neg expressions, the forth level is connected by Compare operators
pNegExpr
  = chainl1 pComExpr pComOp

-- Parser for Com expressions, the fifth level is connected by Term operators
pComExpr
  = chainl1 pTerm pTermOp

-- Parser for Term expressions, the sixth level is connected by Factor operators
pTerm
  = chainl1 pFactor pFactorOp

-- Parser for Factor expressions, the seventh level is connected by UMinus
pFactor
  = try (
      do
        reservedOp "-"
        expr <- pFactor
        return (UMinus expr)
    )
    <|>
    do
      pBaseExpr

-- Parser for Base expressions, the eighth level is (expr), statement variable
-- or constant
pBaseExpr
  = choice [parens pExpr, pIdent, pConst]

pConst, pBool, pNum, pIdent :: Parser Expr

-- Parser for int, float or bool constant
pConst
  = choice [pBool, pNum]

-- Parser for bool constant
pBool
 = do
     reserved "true"
     return (BoolConst True)
   <|>
   do
     reserved "false"
     return (BoolConst False)

-- Parser for int or float constant
pNum
  = try (
      do
        n <- float
        return (FloatConst (double2Float n :: Float))
    )
    <|>
    do
      n <- natural
      return (IntConst (fromInteger n :: Int))

-- Parser for Id in expression
pIdent
  = do
      svar <- pStmtVar
      return (Id svar)

pOrOp, pAndOp, pComOp, pTermOp, pFactorOp :: Parser (Expr -> Expr -> Expr)

-- Parser for Or operator
pOrOp
  = do
      reservedOp "||"
      return Or

-- Parser for And operator
pAndOp
  = do
      reservedOp "&&"
      return And

-- Parser for Compare operators
pComOp
  = choice [pEqualOp, pNotEqualOp, pLessOp, pLessEqualOp,
            pGreaterOp, pGreaterEqualOp]

-- Parser for Term operators
pTermOp
  = choice [pAddOp, pMinusOp]

-- Parser for Factor operators
pFactorOp
  = choice [pMulOp, pDivOp]

pEqualOp, pNotEqualOp, pLessOp :: Parser (Expr -> Expr -> Expr)
pLessEqualOp, pGreaterOp, pGreaterEqualOp :: Parser (Expr -> Expr -> Expr)

-- Parser for Equal operators
pEqualOp
  = do
      reservedOp "="
      return Equal

-- Parser for Not Equal operators
pNotEqualOp
  = do
      reservedOp "!="
      return NotEqual

-- Parser for Less operators
pLessOp
  = do
      reservedOp "<"
      return Less

-- Parser for Less Equal operators
pLessEqualOp
  = do
      reservedOp "<="
      return LessEqual

-- Parser for Greater operators
pGreaterOp
  = do
      reservedOp ">"
      return Greater

-- Parser for Greater Equal operators
pGreaterEqualOp
  = do
      reservedOp ">="
      return GreaterEqual

pAddOp, pMinusOp, pMulOp, pDivOp :: Parser (Expr -> Expr -> Expr)

-- Parser for Add operators
pAddOp
  = do
      reservedOp "+"
      return Add

-- Parser for Minus operators
pMinusOp
  = do
      reservedOp "-"
      return Minus

-- Parser for Mul operators
pMulOp
  = do
      reservedOp "*"
      return Mul

-- Parser for Div operators
pDivOp
  = do
      reservedOp "/"
      return Div

-- Main Parser
pMain :: Parser GoatProg
pMain
  = do
      whiteSpace -- Remove white space in front of the program
      p <- pProg -- Parse the Goat program
      eof -- End of File
      return p

-- Provided by Harald Sondergaard
main :: IO ()
main
  = do
      progname <- getProgName
      args <- getArgs
      task <- checkArgs progname args
      if task == Compile then
        do
          putStrLn "Sorry, cannot generate code yet"
          exitWith ExitSuccess
      else
        if task == Parse then
          do
            let [_, filename] = args
            input <- readFile filename
            let output = runParser pMain () "" input
            case output of
              Right ast -> do { print ast
                              ; putStrLn ""
                              }
              Left  err -> do { putStr "Parse error at "
                              ; print err
                              ; exitWith (ExitFailure 2)
                              }
        else
          do
            let [_, filename] = args
            input <- readFile filename
            let output = runParser pMain () "" input
            case output of
              Right ast -> putStr $ progToStr ast
              Left  err -> do { putStr "Parse error at "
                              ; print err
                              ; exitWith (ExitFailure 2)
                              }

-- Provided by Harald Sondergaard
checkArgs :: String -> [String] -> IO Task
checkArgs _ ['-':_]
  = do
      putStrLn ("Missing filename")
      exitWith (ExitFailure 1)
checkArgs _ [filename]
  = return Compile
checkArgs _ ["-p", filename]
  = return Pprint
checkArgs _ ["-a", filename]
  = return Parse
checkArgs progname _
  = do
      putStrLn ("Usage: " ++ progname ++ " [-p] filename")
      exitWith (ExitFailure 1)
