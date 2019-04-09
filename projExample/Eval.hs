module Eval where

import Syntax
import T ( p )

--------------------------------------------------------------------------
-- A calculator for expressions given by the grammar
--
--    exp     ::=  term + exp | term
--    term    ::=  basic * term | basic
--    basic   ::=  ( exp ) | float
--
--------------------------------------------------------------------------

eval :: String -> Float
eval s
   = ev (p s)

ev :: Exp -> Float
ev (Num x)
   = x
ev (Add e1 e2)
   = ev e1 + ev e2
ev (Mul e1 e2)
   = ev e1 * ev e2

