module Main where

import GoatAST
import Data.Char
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Q
import System.Environment
import System.Exit

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

myReserved
  = [ "begin", "bool", "do", "call", "end", "false", "fi",
      "float", "if", "int", "od", "proc", "read", "ref",
      "then", "true", "val", "while", "write"]

myOpnames
  = [ "+", "-", "*", "/", ":=", "||", "&&",
      "=", "!=", "<", "<=", ">", ">=", "!" ]

pProg :: Parser GoatProgram
pProg
  = do
    procs <- many1 pProc
    return (Program procs)

pProc :: Parser Procedure
pProc
  = do
    reserved "proc"
    header <- pHeader
    (decls, stmts) <- pProcBody
    return (Procedure header decls stmts)

pHeader :: Parser Header
pHeader
  = do
    ident <- identifier
    params <- parens (pParameter `sepBy` comma)
    return (Header ident params)

pParameter :: Parser Parameter
pParameter
  = do
    ind <- pIndicator
    t <- pBaseType
    ident <- identifier
    return (Parameter ind t ident)

pIndicator :: Parser Indicator
pIndicator
  = lexeme (
      try (do { reserved "val"; return Val })
      <|>
      (do { reserved "ref"; return Ref })
    )

pProcBody :: Parser ([Decl], [Stmt])
pProcBody
  = do
    decls <- many pDecl
    reserved "begin"
    -- stmts <- many1 pStmt
    reserved "end"
    return (decls, [])

pDecl :: Parser Decl
pDecl
  = do
    btype <- pBaseType
    var <- pVar
    whiteSpace
    semi
    return (Decl btype var)

pVar:: Parser Var
pVar
  = lexeme (
    try ( do
      { ident <- identifier
      ; shape <- pShape
      ; return (ShapeVar ident shape)
      })
    <|>
    try ( do
      { ident <- identifier
      ; index <- pIndex
      ; return (IndexVar ident index)
      })
    <|>
      ( do
      { ident <- identifier
      ; return (BaseVar ident)
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
      try (do { reserved "bool"; return BoolType })
      <|>
      try (do { reserved "int"; return IntType })
      <|>
      (do { reserved "float"; return FloatType })
    )

-- -----------------------------------------------------------------
-- --  pStmt is the main parser for statements. It wants to recognise
-- --  read and write statements, and assignments.
-- -----------------------------------------------------------------
pStmt, pAssign, pRead, pWrite, pCall, pIf, pIfElse, pWhile :: Parser Stmt


pStmt
  = choice [pAssign, pRead, pWrite, pCall, pIf, pIfElse, pWhile]


pAssign
  = do
    lvalue <- pLValue
    reservedOp ":="
    rvalue <- pExpr
    semi
    return (Assign lvalue rvalue)

pRead
  = do
    reserved "read"
    lvalue <- pLValue
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
    exprs <- parens (many pExpr)
    semi
    return (Call ident exprs)

pIf
  = do
    reserved "if"
    expr <- pExpr
    reserved "then"
    stmts <- many1 pStmt
    reserved "fi"
    return (If expr stmts)

pIfElse
  = do
    reserved "if"
    expr <- pExpr
    reserved "then"
    thsms <- many1 pStmt
    reserved "else"
    elsms <- many1 pStmt
    reserved "fi"
    return (IfElse expr thsms elsms)

pWhile
  = do
    reserved "while"
    expr <- pExpr
    reserved "do"
    stmts <- many1 pStmt
    reserved "od"
    return (While expr stmts)

pLValue :: Parser LValue
pLValue
  = lexeme (
    try ( do
        { ident <- identifier
        ; index <- pIndex
        ; return (DLVal ident index)
        })
      <|>
        ( do
        { ident <- identifier
        ; return (SLVal ident)
        })
    )

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
      v <- pVar
      return (Id v)

pConst :: Parser Expr
pConst
  = choice [pBool, pString, pInt, pFloat]

pBool, pString, pInt, pFloat :: Parser Expr

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

pString 
  = do
      char '"'
      str <- many (satisfy (/= '"'))
      char '"'
      return (StrConst str)

pInt
  = do
    { n <- natural <?> ""
    ; return (IntConst (fromInteger n :: Int))
  }
  <?>
  "integer"

pFloat
  = lexeme (
      try (do { ws <- many1 digit
              ; char '.'
              ; ds <- many1 digit
              ; let val = read (ws ++ ('.' : ds)) :: Float
              ; return (FloatConst val)
          }
      )
          <|>
          (do { ws <- many1 digit
              ; let val = read ws :: Float
              ; return (FloatConst val)
          }
      )
    )

pExpr, pOrExpr, pAndExpr, pNegExpr, pComExpr, pTerm, pFactor, pBaseExpr :: Parser Expr

pExpr
  = chainl1 pOrExpr pOrOp

pOrExpr
  = chainl1 pAndExpr pAndOp

pAndExpr
  -- = chainl1 pNegExpr pNegOp
  = try (do
    { reservedOp "!"
    ; expr <- pNegExpr
    ; return (Neg expr)
    })
    <|>
    do
    { pNegExpr }

pNegExpr
  = chainl1 pComExpr pComOp

pComExpr
  = chainl1 pTerm pTermOp

pTerm
  = chainl1 pFactor pFactorOp

pFactor
  -- = chainl1 pBaseExpr pUminusOp
  = try (do 
      { reservedOp "-"
      ; expr <- pFactor
      ; return (UMinus expr)
      })
    <|>
    do
    { pBaseExpr }

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

-- pNegOp, pUminusOp :: Parser (Expr -> Expr)
-- pNegOp
--   = do
--       reservedOp "!"
--       return Neg

-- pUminusOp
--   = do
--     reservedOp "-"
--     expr <- pFactor
--     return (UMinus expr)

pMain :: Parser GoatProgram
pMain
  = do
      whiteSpace
      p <- pProg
      eof
      return p

main :: IO ()
main
  = do
    { progname <- getProgName
    ; args <- getArgs
    ; checkArgs progname args
    ; input <- readFile (head args)
    ; let output = runParser pMain () "" input
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
  = do
    { putStrLn ("Usage: " ++ progname ++ " filename\n\n")
    ; exitWith (ExitFailure 1)
    }
