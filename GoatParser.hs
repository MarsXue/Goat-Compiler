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
    , Q.opStart         = oneOf "+-*:"
    , Q.opLetter        = oneOf "+-*:"
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

myReserved, myOpnames :: [String]

myReserved
  = [ "begin", "bool", "do", "call", "end", "false", "fi", 
      "float", "if", "int", "od", "proc", "read", "ref", 
      "then", "true", "val", "while", "write", "[", "]" ]

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
    stmts <- many1 pStmt
    reserved "end"
    return (decls, stmts)

pDecl :: Parser Decl
pDecl
  = do
    vtype <- pVarType
    ident <- identifier
    whiteSpace
    semi
    return (Decl vtype ident)

pVarType :: Parser VarType
pVarType
  = lexeme(
    do 
    b <- pBaseType
    try (
      s <- pShape
      return (DVarType b s)
    )
    <|>
    return (SVarType b)
  )

pShape :: Parser Shape
pShape 
  = lexeme (
    try (do 
      { reserved "["
      ; int <- pInt
      ; reserved "]"
      ; reserved "["
      ; int2 <- pInt
      ; reserved "]"
      ; return (DShape int int2)})
    <|>
      (do 
      { reserved "["
      ; int <- pInt
      ; reserved "]"
      ; return (SShape int)
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

pInt :: Parser Expr
pInt
= do
    n <- natural <?> ""
    return (IntConst (fromInteger n :: Int))
  <?>
  "integer"

-- pFloat :: Parser Expr
-- pFloat 
--   = lexeme (
--       try (do { ws <- many1 digit
--               ; char '.'
--               ; ds <- many1 digit 
--               ; let val = read (ws ++ ('.' : ds)) :: Float
--               ; return (Num val)
--           }
--       )
--           <|> 
--           (do { ws <- many1 digit
--               ; let val = read ws :: Float
--               ; return (Num val)
--           }
--       )
--     )

-- -----------------------------------------------------------------
-- --  pStmt is the main parser for statements. It wants to recognise
-- --  read and write statements, and assignments.
-- -----------------------------------------------------------------
-- pStmt, pRead, pWrite, pAsg :: Parser Stmt

-- pStmt 
--   = choice [pRead, pWrite, pAsg]

-- pRead
--   = do
--     reserved "read"
--     lvalue <- pLvalue
--     semi
--     return (Read lvalue)

-- pWrite
--   = do
--     reserved "write"
--     exp <- (pString <|> pExp)
--     semi
--     return (Write exp)

-- pAsg
--   = do
--     lvalue <- pLvalue
--     reservedOp ":="
--     rvalue <- pExp
--     semi
--     return (Assign lvalue rvalue)

-- pAddOp, pMulOp :: Parser (Expr -> Expr -> Expr)

-- pAddOp
--   = do
--     reservedOp "+"
--     return Add

-- pMulOp
--   = do
--     reservedOp "*"
--     return Mul

-- pUminus
--   = do
--     reservedOp "-"
--     exp <- pFactor
--     return (UnaryMinus exp)

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

