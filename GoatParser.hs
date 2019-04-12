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

data Task
  = Compile | Pprint | Parse
  deriving (Show, Eq)

myReserved, myOpnames :: [String]

myReserved
  = [ "begin", "bool", "do", "call", "else", "end", "false",
      "fi", "float", "if", "int", "od", "proc", "read",
      "ref", "then", "true", "val", "while", "write"]

myOpnames
  = [ "+", "-", "*", "/", ":=", "||", "&&",
      "=", "!=", "<", "<=", ">", ">=", "!" ]

pProg :: Parser GoatProg
pProg
  = do
    procs <- many1 pProc
    return (Prog procs)

pProc :: Parser Proc
pProc
  = do
    reserved "proc"
    ident <- identifier
    params <- parens (pParam `sepBy` comma)
    (decls, stmts) <- pProcBody
    return (Proc ident params decls stmts)

pParam :: Parser Param
pParam
  = do
    ind <- pIndicator
    t <- pBaseType
    ident <- identifier
    return (Param ind t ident)

pIndicator :: Parser Indicator
pIndicator
  = lexeme (
    ( do
      { reserved "val"
      ; return Val
      })
    <|>
    ( do
      { reserved "ref"
      ; return Ref
      })
    )

pProcBody :: Parser ([Decl], [Stmt])
pProcBody
  = do
    decls <- many pDecl
    reserved "begin"
    stmts <- many1 pStmt
    reserved "end"
    return (decls, stmts)

pDecl :: Parser Decl
pDecl
  = do
    btype <- pBaseType
    dvar <- pDeclVar
    whiteSpace
    semi
    return (Decl btype dvar)

pDeclVar:: Parser DeclVar
pDeclVar
  = lexeme (
    try ( do
      { ident <- identifier
      ; shape <- pShape
      ; return (ShapeVar ident shape)
      })
    <|>
      ( do
      { ident <- identifier
      ; return (DBaseVar ident)
      })
  )

pStmtVar :: Parser StmtVar
pStmtVar
  = lexeme (
    try ( do
      { ident <- identifier
      ; index <- pIndex
      ; return (IndexVar ident index)
      })
    <|>
      ( do
      { ident <- identifier
      ; return (SBaseVar ident)
      })
  )

pShape :: Parser Shape
pShape =
  brackets (
    try ( do
        { n1 <- natural
        ; lexeme (char ',')
        ; n2 <- natural
        ; return (DShape (fromInteger n1 :: Int) (fromInteger n2 :: Int))
        })
      <|>
        (do
        { n <- natural
        ; return (SShape (fromInteger n :: Int))
        })
  )

pBaseType :: Parser BaseType
pBaseType
  = lexeme (
      ( do
          { reserved "bool"
          ; return BoolType
          })
      <|>
      ( do
          { reserved "int"
          ; return IntType
          })
      <|>
      ( do
        { reserved "float"
        ; return FloatType
        })
    )

-- -----------------------------------------------------------------
-- --  pStmt is the main parser for statements. It wants to recognise
-- --  read and write statements, and assignments.
-- -----------------------------------------------------------------
pStmt, pAssign, pRead, pSWrite, pWrite, pCall, pIf, pWhile :: Parser Stmt


pStmt
  = choice [pAssign, pRead, pWrite, pSWrite, pCall, pIf, pWhile]


pAssign
  = do
    lvalue <- pStmtVar
    reservedOp ":="
    rvalue <- pExpr
    semi
    return (Assign lvalue rvalue)

pRead
  = do
    reserved "read"
    lvalue <- pStmtVar
    semi
    return (Read lvalue)

pWrite
  = try ( do
    reserved "write"
    expr <- pExpr
    semi
    return (Write expr))

pSWrite
  = do
    reserved "write"
    str <- pString
    semi
    return (SWrite str)

pCall
  = do
    reserved "call"
    ident <- identifier
    exprs <- parens (pExpr `sepBy` comma)
    semi
    return (Call ident exprs)

-- pIf
--   = try ( do
--     reserved "if"
--     expr <- pExpr
--     reserved "then"
--     stmts <- many1 pStmt
--     reserved "fi"
--     return (If expr stmts)
--   )
--
-- pIfElse
--   = try ( do
--     reserved "if"
--     expr <- pExpr
--     reserved "then"
--     thsms <- many1 pStmt
--     reserved "else"
--     elsms <- many1 pStmt
--     reserved "fi"
--     return (IfElse expr thsms elsms)
--   )

pIf
  = do
      reserved "if"
      expr <- pExpr
      reserved "then"
      thsms <- many1 pStmt
      elsms <- (
        do
          reserved "else"
          e <- many1 pStmt
          reserved "fi"
          return e
        <|>
        do
          reserved "fi"
          return [])
      return (If expr thsms elsms)

pWhile
  = do
    reserved "while"
    expr <- pExpr
    reserved "do"
    stmts <- many1 pStmt
    reserved "od"
    return (While expr stmts)

pIndex :: Parser Index
pIndex = brackets (
  try ( do
      { e1 <- pExpr
      ; lexeme (char ',')
      ; e2 <- pExpr
      ; return (DIndex e1 e2)
      })
    <|>
      (do
      { e <- pExpr
      ; return (SIndex e)
      })
  )

pIdent :: Parser Expr
pIdent
  = do
    svar <- pStmtVar
    return (Id svar)

pConst :: Parser Expr
pConst
  = choice [pBool, pNum]


pBool, pNum :: Parser Expr

pBool
  = do
    { reserved "true"
    ; return (BoolConst True)
    }
    <|>
    do
    { reserved "false"
    ; return (BoolConst False)
  }

pNum
  = lexeme (
      try ( do
          { n <- float
          ; return (FloatConst (double2Float n :: Float))
      })
      <|>
      ( do
      { n <- natural
      ; return (IntConst (fromInteger n :: Int))
      })
  )

pString :: Parser String
pString
  = lexeme (do
    char '"'
    str <- many (satisfy (not . (`elem` "\"\n\t")))
    char '"'
    return (str))

pExpr, pOrExpr, pAndExpr, pNegExpr, pComExpr, pTerm, pFactor, pBaseExpr :: Parser Expr

pExpr
  = chainl1 pOrExpr pOrOp

pOrExpr
  = chainl1 pAndExpr pAndOp

pAndExpr
  -- = chainl1 pNegExpr pNegOp
  = try ( do
        { reservedOp "!"
        ; expr <- pNegExpr
        ; return (Neg expr)
        })
    <|>
    do
    { pNegExpr
    }

pNegExpr
  = chainl1 pComExpr pComOp

pComExpr
  = chainl1 pTerm pTermOp

pTerm
  = chainl1 pFactor pFactorOp

pFactor
  = try (do
        { reservedOp "-"
        ; expr <- pFactor
        ; return (UMinus expr)
        })
    <|>
    do
    { pBaseExpr
    }

pBaseExpr
  = choice [parens pExpr, pIdent, pConst]

pOrOp, pAndOp, pComOp :: Parser (Expr -> Expr -> Expr)
pOrOp
  = do
    reservedOp "||"
    return Or

pAndOp
  = do
    reservedOp "&&"
    return And

pComOp
  = choice [pEqualOp, pNotEqualOp, pLessOp, pLessEqualOp, pGreaterOp, pGreaterEqualOp]

pTermOp
  = choice [pAddOp, pMinusOp]

pFactorOp
  = choice [pMulOp, pDivOp]

pEqualOp, pNotEqualOp, pLessOp, pLessEqualOp, pGreaterOp, pGreaterEqualOp :: Parser (Expr -> Expr -> Expr)

pEqualOp
  = do
    reservedOp "="
    return Equal

pNotEqualOp
  = do
    reservedOp "!="
    return NotEqual

pLessOp
  = do
    reservedOp "<"
    return Less

pLessEqualOp
  = do
    reservedOp "<="
    return LessEqual

pGreaterOp
  = do
    reservedOp ">"
    return Greater

pGreaterEqualOp
  = do
    reservedOp ">="
    return GreaterEqual

pAddOp, pMinusOp, pMulOp, pDivOp :: Parser (Expr -> Expr -> Expr)

pAddOp
  = do
    reservedOp "+"
    return Add

pMinusOp
  = do
    reservedOp "-"
    return Minus

pMulOp
  = do
    reservedOp "*"
    return Mul

pDivOp
  = do
    reservedOp "/"
    return Div

pMain :: Parser GoatProg
pMain
  = do
      whiteSpace
      p <- pProg
      eof
      return p

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
                              -- ; putStr $ progToString ast
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
              Right ast -> putStr $ progToString ast
              Left  err -> do { putStr "Parse error at "
                              ; print err
                              ; exitWith (ExitFailure 2)
                              }

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
