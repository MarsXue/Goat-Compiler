module Main where

import Data.Char
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Q
import System.Environment
import System.Exit

type Parser a
   = Parsec [Char] Int a

data Exp    
   = Num Float | Add Exp Exp | Mul Exp Exp 
     deriving (Eq,Show)

lexer :: Q.TokenParser Int
lexer
   = Q.makeTokenParser
     (emptyDef
     { Q.commentStart    = "{"
     , Q.commentEnd      = "}"
     , Q.commentLine     = ""
     , Q.nestedComments  = True
     , Q.identStart      = letter
     , Q.opStart         = oneOf "+*"
     , Q.opLetter        = oneOf "+*"
     , Q.reservedOpNames = ["+","*"]
     })

whiteSpace = Q.whiteSpace lexer
lexeme     = Q.lexeme lexer
natural    = Q.natural lexer
parens     = Q.parens lexer
reservedOp = Q.reservedOp lexer

pFloat :: Parser Exp
pFloat 
   = lexeme (try
              (do { ws <- many1 digit
                  ; char '.'
                  ; ds <- many1 digit 
                  ; let val = read (ws ++ ('.' : ds)) :: Float
                  ; return (Num val)
                  }
              )
             <|> 
              (do { ws <- many1 digit
                  ; modifyState (+1)
                  ; let val = read ws :: Float
                  ; return (Num val)
                  }
              )
           )

pBasic :: Parser Exp
pBasic
   = pFloat <|> parens pExp
            
pTerm :: Parser Exp
pTerm 
   = do { e <- pBasic 
        ; (do { e' <- pMulTerm
              ; return (Mul e e')
              }
           ) <|> (return e)
        }
       
pMulTerm :: Parser Exp
pMulTerm 
   = do { reservedOp "*"
        ; pTerm
        }

pExp :: Parser Exp
pExp
   = do { e <- pTerm 
        ; (do { e' <- pAddExp
              ; return (Add e e')
              }
           ) <|> (return e)
        }

pAddExp :: Parser Exp
pAddExp
   = do { reservedOp "+"
        ; pExp
        }

pMain :: Parser (Int,Exp)
pMain 
   = do { whiteSpace
        ; e <- pExp
        ; eof
        ; n <- getState
        ; return (n,e)
        }

-----------------------------------------------------------------
--
-- main
--
-----------------------------------------------------------------
main :: IO ()
main
  = do { progname <- getProgName
       ; args <- getArgs
       ; checkArgs progname args
       ; input <- readFile (head args)
       ; let output = runParser pMain 0 "" input
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
   = do { putStrLn ("Usage: " ++ progname ++ " filename\n\n")
       ; exitWith (ExitFailure 1)
       }

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
e11 = " 1.2 + { Include a comment } 456"
e12 = " 1.2 + { Allow { for } nested comments? } 456"

