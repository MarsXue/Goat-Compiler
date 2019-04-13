module GoatFormat where

import GoatAST

-- Program to String
progToString :: GoatProg -> String
progToString (Prog []) = ""
progToString (Prog (p:procs))
  -- If procs is non-empty, there should be a newline
  = procToString p ++ "\n" ++ progToString (Prog procs)

-- Procedure to String
procToString :: Proc -> String
procToString (Proc ident params decls stmts)
  -- TOKEN "proc"
  = "proc " 
  -- Header with identifier and parameters in parentheses
  ++ ident ++ " (" ++ paramsToString params ++ ")\n"
  -- Declarations
  ++ declsToString decls
  -- TOKEN "begin"
  ++ "begin\n"
  -- Statements with one indentation level
  ++ stmtsToString 1 stmts
  -- TOKEN "end"
  ++ "end\n"

-- Parameters to String (recursive)
paramsToString :: [Param] -> String
paramsToString [] = ""
-- Last parameter
paramsToString (p:[]) = (paramToString p)
-- Paramerter except the last one should be followed by a comma
paramsToString (p:params)
  = (paramToString p) ++ ", " ++ (paramsToString params)

-- Parameter to String
paramToString :: Param -> String
paramToString (Param Val basetype ident)
  = "val " ++ (baseTypeToString basetype) ++ " " ++ ident
paramToString (Param Ref basetype ident)
  = "ref " ++ (baseTypeToString basetype) ++ " " ++ ident

-- Declarations to String (recursive)
declsToString :: [Decl] -> String
declsToString [] = ""
declsToString (decl:decls)
  = (declToString decl) ++ "\n" ++ (declsToString decls)

-- Declaration to String
declToString :: Decl -> String
declToString (Decl baseType declVar)
  -- Declaration is default with 4 spaces indentation
  = "    "
  -- Base type for declaration
  ++ baseTypeToString baseType
  ++ " "
  -- Declaration variable with semi colon at the end
  ++ declVarToString declVar ++ ";"

-- Base type to String
baseTypeToString :: BaseType -> String
baseTypeToString baseType
  | baseType == BoolType  = "bool"
  | baseType == IntType   = "int"
  | baseType == FloatType = "float"

-- Declaration variable to String
declVarToString :: DeclVar -> String
declVarToString (DBaseVar ident) = ident
declVarToString (ShapeVar ident shape)
  = ident ++ shapeToString shape

-- Shape to String
shapeToString :: Shape -> String
shapeToString (SShape a)
  = "[" ++ (show a) ++ "]"
shapeToString (DShape a b)
  = "[" ++ (show a) ++ "," ++ (show b) ++ "]"

-- Statements to String (recursive)
stmtsToString :: Int -> [Stmt] -> String
stmtsToString _ [] = ""
stmtsToString a (s:stmts)
  = (stmtToString a s) ++ (stmtsToString a stmts)

-- Statement to String
stmtToString :: Int -> Stmt -> String
-- Assign statement
stmtToString a (Assign stmtVar expr)
  = (spaces a) ++ (stmtVarToString stmtVar)
  ++ " := " ++ (exprToString False expr) ++ ";\n"
-- Read statement
stmtToString a (Read stmtVar)
  = (spaces a) ++ "read " ++ (stmtVarToString stmtVar) ++ ";\n"
-- Write statement
stmtToString a (Write expr)
  = (spaces a) ++ "write " ++ (exprToString False expr) ++ ";\n"
-- Write string statement
stmtToString a (SWrite string)
  = (spaces a) ++ "write \"" ++ string ++ "\";\n"
-- Call statement
stmtToString a (Call ident exprs)
  = (spaces a) ++ "call " ++ ident ++ (exprsToString exprs) ++ ";\n"
-- If statement
stmtToString a (If expr stmts1 stmts2)
  = (spaces a)
  -- TOKEN "if"
  ++ "if "
  ++ (exprToString False expr)
  -- TOKEN "then"
  ++ " then\n"
  ++ (stmtsToString (a+1) stmts1)
  -- If stmts2 is non-empty list, there is a else statement
  ++ rest
  ++ (spaces a)
  -- TOKEN "fi"
  ++ "fi\n"
  where
      rest
        | length stmts2 == 0  = ""
        | otherwise           =  (spaces a)
                              -- TOKEN "else"
                              ++ "else\n"
                              ++ (stmtsToString (a+1) stmts2)
stmtToString a (While expr stmts)
  = (spaces a)
  -- TOKEN "while"
  ++ "while "
  ++ (exprToString False expr)
  -- TOKEN "do"
  ++ " do\n"
  ++ (stmtsToString (a+1) stmts)
  ++ (spaces a)
  -- TOKEN "od"
  ++ "od\n"

-- Statement variable to String
stmtVarToString :: StmtVar -> String
stmtVarToString (SBaseVar ident) = ident
stmtVarToString (IndexVar ident index) = ident ++ (indexToString index)

-- Index to String
indexToString :: Index -> String
indexToString (SIndex expr)
  = "[" ++ (exprToString False expr) ++ "]"
indexToString (DIndex expr1 expr2)
  = "[" ++ (exprToString False expr1) ++ ","
  ++ (exprToString False expr2) ++ "]"

-- Expressions to String (recursive)
exprsToString :: [Expr] -> String
exprsToString [] = ""
exprsToString (e:exprs) = "(" ++ exprToString False e ++ rest
  where
      rest
        | length exprs > 0 = ", " ++ exprsToString exprs
        | otherwise        = ")"

-- Expression to String
exprToString :: Bool -> Expr -> String
exprToString _ (Id stmtVar) = stmtVarToString stmtVar
-- Constant expression
exprToString _ (BoolConst True) = "true"
exprToString _ (BoolConst False) = "false"
exprToString _ (IntConst int) = show int
exprToString _ (FloatConst float) = show float
-- Binary operation expression
exprToString bool (Add expr1 expr2)
  = genBinopString bool " + " expr1 expr2
exprToString bool (Minus expr1 expr2)
  = genBinopString bool " - " expr1 expr2
exprToString bool (Mul expr1 expr2)
  = genBinopString bool " * " expr1 expr2
exprToString bool (Div expr1 expr2)
  = genBinopString bool " / " expr1 expr2
exprToString bool (Or expr1 expr2)
  = genBinopString bool " || " expr1 expr2
exprToString bool (And expr1 expr2)
  = genBinopString bool " && " expr1 expr2
exprToString bool (Equal expr1 expr2)
  = genBinopString bool " = " expr1 expr2
exprToString bool (NotEqual expr1 expr2)
  = genBinopString bool " != " expr1 expr2
exprToString bool (Less expr1 expr2)
  = genBinopString bool " < " expr1 expr2
exprToString bool (LessEqual expr1 expr2)
  = genBinopString bool " <= " expr1 expr2
exprToString bool (Greater expr1 expr2)
  = genBinopString bool " > " expr1 expr2
exprToString bool (GreaterEqual expr1 expr2)
  = genBinopString bool " >= " expr1 expr2
-- Unary operation expression
exprToString _ (Neg expr)
  = "!" ++ (exprToString True expr)
exprToString _ (UMinus expr)
  = "-" ++ (exprToString True expr)

---------------------------------
-- Helper function for GoatFormat
---------------------------------

-- Generate binary operation string
genBinopString :: Bool -> String -> Expr -> Expr -> String
genBinopString bool symbol expr1 expr2
  | bool      = "(" ++ middle ++ ")"
  | otherwise = middle
  where
    middle = exprToString True expr1 ++ symbol ++ exprToString True expr2

-- Print spaces based on the input indentation level
spaces :: Int -> String
spaces 0 = ""
spaces n = "    " ++ spaces (n-1)