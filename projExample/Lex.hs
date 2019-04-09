module Lex where

import Data.Char

data Token
   = NUM String | ADD | MUL | LPA | RPA
     deriving (Eq,Show)

--------------------------------------------------------------------------
-- Lexical analysis:

scan :: String -> [Token]
scan [] 
   = []
scan (c:cs)
   | isSpace c = scan cs
   | isDigitOrStop c = checked digsAndStops : scan afterDigs
   | c == '('  = LPA : scan cs
   | c == ')'  = RPA : scan cs
   | c == '+'  = ADD : scan cs
   | c == '*'  = MUL : scan cs
   | otherwise = error errorMessage
   where
      isDigitOrStop c           = isDigit c || c == '.'
      (digsAndStops, afterDigs) = span isDigitOrStop (c:cs)
      checked s                 = if validNum s then NUM s else
                                     error invalidNumMessage
      errorMessage              = "lexical error at " ++ [c]
      invalidNumMessage         = "invalid number"

validNum :: String -> Bool
validNum s
   | head s == '.' = False
   | last s == '.' = False
   | otherwise     = length (filter (=='.') s) <= 1

