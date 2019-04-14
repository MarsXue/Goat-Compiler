---------------------------------------------------------
-- Programming Language Implementation COMP90045 Project1
-- Implemented by Shjie Liu, Wenqing Xue, Minjian Chen
---------------------------------------------------------

module GoatFormat where

import GoatAST

-- Program to String
progToStr :: GoatProg -> String
progToStr (Prog []) = ""
progToStr (Prog (p:procs))
  -- There should be a newline between procedures
  = procToStr p ++ "\n" ++ progToStr (Prog procs)

-- Procedure to String
procToStr :: Proc -> String
procToStr (Proc ident params decls stmts)
  -- TOKEN "proc"
  = "proc "
  -- Header with identifier and parameters in parentheses
  ++ ident ++ " (" ++ paramsToStr params ++ ")\n"
  -- Declarations
  ++ declsToStr decls
  -- TOKEN "begin"
  ++ "begin\n"
  -- Statements with one indentation level
  ++ stmtsToStr 1 stmts
  -- TOKEN "end"
  ++ "end\n"

-- Parameters to String (recursive)
paramsToStr :: [Param] -> String
paramsToStr []         = ""
-- Last parameter
paramsToStr [p]        = paramToStr p
-- Paramerter except the last one should be followed by a comma
paramsToStr (p:params) = paramToStr p ++ ", " ++ paramsToStr params

-- Parameter to String
paramToStr :: Param -> String
paramToStr (Param Val basetype ident)
  = "val " ++ baseTypeToStr basetype ++ " " ++ ident
paramToStr (Param Ref basetype ident)
  = "ref " ++ baseTypeToStr basetype ++ " " ++ ident

-- Declarations to String (recursive)
declsToStr :: [Decl] -> String
declsToStr []           = ""
declsToStr (decl:decls) = declToStr decl ++ "\n" ++ declsToStr decls

-- Declaration to String
declToStr :: Decl -> String
declToStr (Decl baseType declVar)
  -- Declaration is default with 4 space indentation
  = "    "
  -- Base type for declaration
  ++ baseTypeToStr baseType
  ++ " "
  -- Declaration variable with semi colon at the end
  ++ declVarToStr declVar ++ ";"

-- Base type to String
baseTypeToStr :: BaseType -> String
baseTypeToStr baseType
  | baseType == BoolType  = "bool"
  | baseType == IntType   = "int"
  | baseType == FloatType = "float"

-- Declaration variable to String
declVarToStr :: DeclVar -> String
declVarToStr (DBaseVar ident)       = ident
declVarToStr (ShapeVar ident shape) = ident ++ shapeToStr shape

-- Shape to String
shapeToStr :: Shape -> String
shapeToStr (SArray a)   = "[" ++ show a ++ "]"
shapeToStr (SMatrix a b) = "[" ++ show a ++ "," ++ show b ++ "]"

-- Statements to String (recursive)
stmtsToStr :: Int -> [Stmt] -> String
stmtsToStr _ []           = ""
stmtsToStr i (stmt:stmts) = stmtToStr i stmt ++ stmtsToStr i stmts

-- Statement to String
stmtToStr :: Int -> Stmt -> String
-- Assign statement
stmtToStr i (Assign stmtVar expr)
  = space i ++ stmtVarToStr stmtVar ++ " := " ++ exprToStr False expr ++ ";\n"
-- Read statement
stmtToStr i (Read stmtVar)
  = space i ++ "read " ++ stmtVarToStr stmtVar ++ ";\n"
-- Write statement
stmtToStr i (Write expr)
  = space i ++ "write " ++ exprToStr False expr ++ ";\n"
-- Write string statement
stmtToStr i (SWrite string)
  = space i ++ "write \"" ++ string ++ "\";\n"
-- Call statement
stmtToStr i (Call ident exprs)
  = space i ++ "call " ++ ident ++ exprsToStr exprs ++ ";\n"
-- If statement
stmtToStr i (If expr stmts1 stmts2)
  = space i
  -- TOKEN "if"
  ++ "if "
  ++ exprToStr False expr
  -- TOKEN "then"
  ++ " then\n"
  ++ stmtsToStr (i + 1) stmts1
  -- If stmts2 is non-empty list, there is a else statement
  ++ rest
  ++ space i
  -- TOKEN "fi"
  ++ "fi\n"
  where
    rest
      | null stmts2 = ""
      | otherwise   = space i
                    -- TOKEN "else"
                    ++ "else\n"
                    ++ stmtsToStr (i + 1) stmts2
stmtToStr i (While expr stmts)
  = space i
  -- TOKEN "while"
  ++ "while "
  ++ exprToStr False expr
  -- TOKEN "do"
  ++ " do\n"
  ++ stmtsToStr (i + 1) stmts
  ++ space i
  -- TOKEN "od"
  ++ "od\n"

-- Statement variable to String
stmtVarToStr :: StmtVar -> String
stmtVarToStr (SBaseVar ident)       = ident
stmtVarToStr (IndexVar ident index) = ident ++ indexToStr index

-- Index to String
indexToStr :: Index -> String
indexToStr (IArray expr) = "[" ++ exprToStr False expr ++ "]"
indexToStr (IMatrix expr1 expr2)
  = "[" ++ exprToStr False expr1 ++ "," ++ exprToStr False expr2 ++ "]"

-- Expressions to String (recursive)
exprsToStr :: [Expr] -> String
exprsToStr [] = ""
exprsToStr (e:exprs) = "(" ++ exprToStr False e ++ rest
  where
    rest
      | not (null exprs) = ", " ++ exprsToStr exprs
      | otherwise        = ")"

-- Expression to String
exprToStr :: Bool -> Expr -> String
exprToStr _ (Id stmtVar)                  = stmtVarToStr stmtVar
-- Constant expression
exprToStr _ (BoolConst True)              = "true"
exprToStr _ (BoolConst False)             = "false"
exprToStr _ (IntConst int)                = show int
exprToStr _ (FloatConst float)            = show float
-- Binary operation expression
exprToStr bool (Add expr1 expr2)          = binopToStr bool " + " expr1 expr2
exprToStr bool (Minus expr1 expr2)        = binopToStr bool " - " expr1 expr2
exprToStr bool (Mul expr1 expr2)          = binopToStr bool " * " expr1 expr2
exprToStr bool (Div expr1 expr2)          = binopToStr bool " / " expr1 expr2
exprToStr bool (Or expr1 expr2)           = binopToStr bool " || " expr1 expr2
exprToStr bool (And expr1 expr2)          = binopToStr bool " && " expr1 expr2
exprToStr bool (Equal expr1 expr2)        = binopToStr bool " = " expr1 expr2
exprToStr bool (NotEqual expr1 expr2)     = binopToStr bool " != " expr1 expr2
exprToStr bool (Less expr1 expr2)         = binopToStr bool " < " expr1 expr2
exprToStr bool (LessEqual expr1 expr2)    = binopToStr bool " <= " expr1 expr2
exprToStr bool (Greater expr1 expr2)      = binopToStr bool " > " expr1 expr2
exprToStr bool (GreaterEqual expr1 expr2) = binopToStr bool " >= " expr1 expr2
-- Unary operation expression
exprToStr _ (Neg expr)                    = "!" ++ exprToStr True expr
exprToStr _ (UMinus expr)                 = "-" ++ exprToStr True expr

---------------------------------
-- Helper function for GoatFormat
---------------------------------

-- Generate binary operation string
binopToStr :: Bool -> String -> Expr -> Expr -> String
binopToStr bool symbol expr1 expr2
  | bool      = "(" ++ middle ++ ")"
  | otherwise = middle
  where
    middle = exprToStr True expr1 ++ symbol ++ exprToStr True expr2

-- Print space based on the input indentation level
space :: Int -> String
space 0 = ""
space n = "    " ++ space (n - 1)
