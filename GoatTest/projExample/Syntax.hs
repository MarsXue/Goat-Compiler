module Syntax where

--------------------------------------------------------------------
--
-- Data types for ASTs
--
--------------------------------------------------------------------

data Exp
   = Num Float | Add Exp Exp | Mul Exp Exp
     deriving (Eq,Show)

--------------------------------------------------------------------
--
-- This module could also contain various syntax-oriented functions, 
-- used in semantic analysis.
--
--------------------------------------------------------------------

