module GoatParser where

import GoatAST
import Data.Char
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Q
import System.Environment
import System.Exit

type Parser a 
    = Parsec [Char] () a

data Exp
    = Num Float | Add Exp Exp | Mul Exp Exp
    deriving (Show, Eq)

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
    =  ["begin", "bool", "do", "end", "false", "fi",
        "float", "if", "int", "od", "proc", "read", 
        "ref", "then", "true", "val", "while", "write"]

myOpnames 
    =  ["+", "-", "*", "/" ":=", "||", "&&", 
        "=", "!=", "<", "<=", ">", ">=", "!"]

-----------------------------------------------------------------
--  pProg is the topmost parsing function. It looks for a program
--  header "proc main()", followed by the program body.
-----------------------------------------------------------------
pProg :: Parser GoatProgram
pProg
    = do
        reserved "proc"
        reserved "main"
        parens (return ())
        (decls, stmts) <- pProgBody
        return (Program decls stmts)

-----------------------------------------------------------------
--  pProgBody looks for a sequence of declarations followed by a
--  sequence of statements.
-----------------------------------------------------------------
pProgBody :: Parser ([Decl], [Stmt])
pProgBody
    = do
        decls <- many pDecl
        reserved "begin"
        stmts <- many1 pStmt
        reserved "end"
        return (decls, stmts)

pDecl :: Parser Decl
pDecl
    = do
        basetype <- pBaseType
        ident <- identifier
        whiteSpace
        semi
        return (Decl ident basetype)

pBaseType :: Parser basetype
pBaseType
    = lexeme (
        try (do { reserved "bool"; return BoolType })
        <|>
        try (do { reserved "int"; return IntType })
        <|>
        (do { reserved "float"; return FloatType })
    )

-----------------------------------------------------------------
--  pStmt is the main parser for statements. It wants to recognise
--  read and write statements, and assignments.
-----------------------------------------------------------------
pStmt, pRead, pWrite, pAsg :: Parser Stmt

pStmt 
    = choice [pRead, pWrite, pAsg]

pRead
    = do
        reserved "read"
        lvalue <- pLvalue
        semi
        return (Read lvalue)

pWrite
    = do
        reserved "write"
        exp <- (pString <|> pExp)

pAsg
    = do
        lvalue <- pLvalue
        reservedOp ":="
        rvalue <- pExp
        semi
        return (Assign lvalue rvalue)

pAddOp, pMulOp :: Parser (Expr -> Expr -> Expr)

pAddOp
    = do
        reservedOp "+"
        return Add

pMulOp
    = do
        reservedOp "*"
        return Mul