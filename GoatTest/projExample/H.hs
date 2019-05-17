module H where

import Lex ( Token(..), scan )

--------------------------------------------------------------------------
-- Syntax analysis for a small expression language
--
--    exp     ::=  term + exp | term
--    term    ::=  basic * term | basic
--    basic   ::=  ( exp ) | float 
--------------------------------------------------------------------------

type Parser a = [Token] -> [(a, [Token])]

data Exp
   = Num Float | Add Exp Exp | Mul Exp Exp
     deriving (Eq,Show)

--------------------------------------------------------------------------
-- Parser combinators:

-- The parser p1 <|> p2 works like p1 unless p1 fails, in which case
-- it works like p2.

(<|>) :: Parser a -> Parser a -> Parser a
(p1 <|> p2) toks
   = p1 toks ++ p2 toks

-- The parser p1 <&> (\x -> p2) works like p1, followed by p2 working
-- on the output produced by p1.

(<&>) :: Parser a -> (a -> Parser b) -> Parser b
(p <&> f) toks
   = concat [ f val rest | (val, rest) <- p toks ]

-- The empty parser 'produce r' consumes no tokens but produces r.

produce :: a -> Parser a
produce r toks
   = [(r, toks)]

-- Various specialised parsers

pAdd :: Parser ()
pAdd (ADD : toks)
   = [((), toks)]
pAdd _ 
   = []

pMul :: Parser ()
pMul (MUL : toks)
   = [((), toks)]
pMul _ 
   = []

pLeftPar :: Parser ()
pLeftPar (LPA : toks)
   = [((), toks)]
pLeftPar _ 
   = []

pRightPar :: Parser ()
pRightPar (RPA : toks)
   = [((), toks)]
pRightPar _ 
   = []

-- pFloat recognises floating point literals.

pFloat :: Parser Float
pFloat (NUM x : toks)
   = [(read x :: Float, toks)]
pFloat _
   = []

-- pExp recognises an expression.

pExp :: Parser Exp
pExp
   = pTerm <&> \e1 -> (pAddExp e1 <|> produce e1)

pAddExp :: Exp -> Parser Exp
pAddExp e1
   = pAdd <&> \_  ->
     pExp  <&> \e2 ->
     produce (Add e1 e2)

pTerm :: Parser Exp
pTerm
   = pBasic <&> \e1 -> (pMulTerm e1 <|> produce e1)

pMulTerm :: Exp -> Parser Exp
pMulTerm e1
   = pMul  <&> \_  ->
     pTerm <&> \e2 ->
     produce (Mul e1 e2)

pBasic :: Parser Exp
pBasic
   = pBrackExp <|> pNum

pBrackExp :: Parser Exp
pBrackExp
   = pLeftPar  <&> \_ ->
     pExp      <&> \e ->
     pRightPar <&> \_ ->
     produce e

pNum :: Parser Exp
pNum
   = pFloat <&> \x ->
     produce (Num x)

parse :: String -> Exp
parse str
   | pExp tokens == [] = error "Parse error in input"
   | rest /= []        = error "Parse error in input"
   | otherwise         = e
   where
      tokens    = scan str
      (e, rest) = head (pExp tokens)

--------------------------------------------------------------------------
-- Tests:

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
