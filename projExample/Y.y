{
module Y ( p, pFile, echo, echoFile ) where

import Syntax
import Data.Char (isSpace, isDigit)
import Data.List (intersperse)
}

--------------------------------------------------------------------------
--  Syntax analysis for a small expression language 
--
--     exp    ::=  term + exp | term
--     term   ::=  basic * term | basic
--     basic  ::=  ( exp ) | float
--
--  Actually the grammar we use is simpler, and left-recursive,
--  which is suitable for an LALR parser. We assume + and * are
--  left-associative. We indicate, through the order of the %left
--  associativity declarations, that * binds tighter than +.
--  Then the grammar is simply:
--
--     exp  ::=  exp + exp | exp * exp | ( exp ) | float
--
--------------------------------------------------------------------------

%name generatedParser
%tokentype { Token }
%error { parseError }

%left '+'
%left '*'

%token
  '+'   { ADD }
  '*'   { MUL }
  '('   { LPA }
  ')'   { RPA }
  num   { NUM $$ }
%%

Exp :: { Exp }
  : num           { Num (read $1) }
  | Exp '+' Exp   { Add $1 $3 }
  | Exp '*' Exp   { Mul $1 $3 }
  | '(' Exp ')'   { $2 }

{
parseError :: [Token] -> a
parseError tks
  = error $ "Parse error at token " ++
      spaced_list (map show (take 12 tks)) ++ " ..."

data Token
  = NUM String | ADD | MUL | LPA | RPA
    deriving (Eq,Show)

lexer :: String -> [Token]
lexer []
   = []
lexer (c:cs)
   | isSpace c = lexer cs
   | isDigitOrStop c = checked digsAndStops : lexer afterDigs
   | c == '('  = LPA : lexer cs
   | c == ')'  = RPA : lexer cs
   | c == '+'  = ADD : lexer cs
   | c == '*'  = MUL : lexer cs
   | otherwise = error errorMessage
   where
      isDigitOrStop c           = isDigit c || c == '.'
      (digsAndStops, afterDigs) = span isDigitOrStop (c:cs)
      checked s                 = if validNum s then NUM s else
                                     error invalidNumMessage
      errorMessage              = "lexical error at " ++ [c]
      invalidNumMessage         = "invalid number"

-- 'validNum s' is true if s denotes a valid number, that is, it has
-- at most one occurrence of a full stop, and the stop does not begin
-- or end the string.

validNum :: String -> Bool
validNum s
   | head s == '.' = False
   | last s == '.' = False
   | otherwise     = length (filter (=='.') s) <= 1

spaced_list :: [String] -> String
spaced_list
  = concat . intersperse " "

----------------------------------------------------------------------
-- The parser
----------------------------------------------------------------------

p :: String -> Exp
p s
  = generatedParser (lexer s)

pFile :: FilePath -> IO Exp
pFile fp
 = do e <- readFile fp
      return (p e)

echo :: String -> String
echo s
  = pretty (p s)

echoFile :: FilePath -> IO ()
echoFile fp
 = do e <- readFile fp
      putStr (pretty (p e))
      return ()

----------------------------------------------------------------------
-- Pretty printing expressions:
----------------------------------------------------------------------

pretty :: Exp -> String
pretty (Num k)
  = show k
pretty (Add e1 e2)
  = pretty e1 ++ " + " ++ pretty e2
pretty (Mul e1@(Add _ _) e2@(Add _ _))
  = parenthesize e1 ++ " * " ++ parenthesize e2
pretty (Mul e1@(Add _ _) e2)
  = parenthesize e1 ++ " * " ++ pretty e2
pretty (Mul e1 e2@(Add _ _))
  = pretty e1 ++ " * " ++ parenthesize e2
pretty (Mul e1 e2)
  = pretty e1 ++ " * " ++ pretty e2

parenthesize e
  = '(' : pretty e ++ ")"

--------------------------------------------------------------------------
-- Tests:
--------------------------------------------------------------------------

e0 = "123"
e1 = "123.456"
e2 = "1.3 + 4.2"
e3 = "1.3 + 4.2 + 7.345"
e4 = "1.3 * 4.2"
e5 = "1.3 * 4.2 * 7.345"
e6 = "1.3 * 4.2 + 7.345"
e7 = "1.3 + 4.2 * 7.345"
e8 = "1.3 + (4.2 + 5.8) * 7.345"
e9 = "1.3 + ((4.2 + 5.8) * 7.345 + 9)"
e10 = "((((1.3))))"

e20 = "(4 + 5) + (6 + 7 + 8)"
e21 = "(4 + 5) * (6 + 7 + 8)"
e22 = "(4 * 5) + (6 * 7 * 8)"
e23 = "(4 * 5) + (6 * 7 * (8 + 9))"
e24 = "1 * ((2 + 3) + (4 * 5)) + ((6 + 7) * (8 * 9))"

}
