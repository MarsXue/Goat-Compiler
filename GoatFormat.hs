module GoatFormat where

import GoatAST

progToString :: GoatProg -> String
progToString (Prog []) = ""
progToString (Prog (p:procs)) =
  procToString p ++ "\n" ++ progToString (Prog procs)
  -- check whether procs list is empty, to determine whether there should be a new line

procToString :: Proc -> String
procToString (Proc ident params decls stmts) =
    "proc " ++ ident ++ " (" ++
    paramsToString params ++ ")\n" ++
    declsToString decls ++
    "begin\n" ++
    stmtsToString 1 stmts ++
    "end\n"

paramsToString :: [Param] -> String
paramsToString [] = ""
paramsToString (p:[]) = (paramToString p)
paramsToString (p:params) = (paramToString p) ++ ", " ++ (paramsToString params)

paramToString :: Param -> String
paramToString (Param Val basetype ident) =
    "val" ++ " " ++ (baseTypeToString basetype) ++ " " ++ ident
paramToString (Param Ref basetype ident) =
    "ref" ++ " " ++ (baseTypeToString basetype) ++ " " ++ ident

declsToString :: [Decl] -> String
declsToString [] = ""
declsToString (decl:decls) =
  (declToString decl) ++ "\n" ++ (declsToString decls)

declToString :: Decl -> String
declToString (Decl baseType declVar) =
  "    " ++ baseTypeToString baseType ++ " " ++
  declVarToString declVar ++ ";"

baseTypeToString :: BaseType -> String
baseTypeToString baseType
  |baseType == BoolType    ="bool"
  |baseType == IntType     ="int"
  |baseType == FloatType   ="float"

declVarToString :: DeclVar -> String
declVarToString (DBaseVar ident) = ident
declVarToString (ShapeVar ident shape) =
  ident ++ shapeToString shape

shapeToString :: Shape -> String
shapeToString (SShape a) =
  "[" ++ (show a) ++ "]"
shapeToString (DShape a b) =
  (shapeToString (SShape a)) ++ (shapeToString (SShape b))

spaces :: Int -> String
spaces 0 = ""
spaces n = "    " ++ spaces (n-1)

--statements
stmtsToString :: Int -> [Stmt] -> String
stmtsToString _ [] = ""
stmtsToString a (s:stmts) =
  (stmtToString a s) ++ (stmtsToString a stmts)

stmtToString :: Int -> Stmt -> String
stmtToString a (Assign stmtVar expr) =
  (spaces a) ++ (stmtVarToString stmtVar) ++ " := " ++ (exprToString False expr) ++ ";\n"
stmtToString a (Read stmtVar) =
  (spaces a) ++ "read " ++ (stmtVarToString stmtVar) ++ ";\n"
stmtToString a (Write expr) =
  (spaces a) ++ "write " ++ (exprToString False expr) ++ ";\n"
stmtToString a (SWrite string) =
  (spaces a) ++ "write \"" ++ string ++ "\";\n"
stmtToString a (Call ident exprs) =
  (spaces a) ++ "call " ++ ident ++ (exprsToString exprs) ++ ";\n"
stmtToString a (If expr stmts1 stmts2) =
  (spaces a) ++ "if " ++ (exprToString False expr) ++ " then\n" ++
  (stmtsToString (a+1) stmts1) ++
  rest ++
  (spaces a) ++ "fi\n"
  where
      rest
        |(length stmts2) == 0     = ""
        |otherwise                = (spaces a) ++ "else\n" ++ (stmtsToString (a+1) stmts2)
stmtToString a (While expr stmts) =
  (spaces a) ++ "while " ++ (exprToString False expr) ++ " do\n" ++
  (stmtsToString (a+1) stmts) ++
  (spaces a) ++ "od\n"

stmtVarToString :: StmtVar -> String
stmtVarToString (SBaseVar ident) = ident
stmtVarToString (IndexVar ident index) = ident ++ (indexToString index)

indexToString :: Index -> String
indexToString (SIndex expr) = "[" ++ (exprToString False expr) ++ "]"
indexToString (DIndex expr1 expr2) = (indexToString (SIndex expr1)) ++ (indexToString (SIndex expr2))

exprsToString :: [Expr] -> String
exprsToString [] = ""
exprsToString (e:exprs) = "(" ++ exprToString False e ++ rest
  where
      rest
        |length(exprs) > 0   =", " ++ exprsToString exprs
        |otherwise           =")"

genBinopString :: Bool -> String -> Expr -> Expr -> String
genBinopString bool symbol expr1 expr2
  |bool                 = "(" ++ middle ++ ")"
  |otherwise            = middle
  where
    middle = exprToString True expr1 ++ symbol ++ exprToString True expr2

exprToString :: Bool -> Expr -> String
exprToString _ (Id stmtVar) = stmtVarToString stmtVar
exprToString _ (BoolConst True) = "true"
exprToString _ (BoolConst False) = "false"
exprToString _ (IntConst int) = show int
exprToString _ (FloatConst float) = show float
exprToString bool (Add expr1 expr2) = genBinopString bool " + " expr1 expr2
exprToString bool (Minus expr1 expr2) = genBinopString bool " - " expr1 expr2
exprToString bool (Mul expr1 expr2) = genBinopString bool " * " expr1 expr2
exprToString bool (Div expr1 expr2) = genBinopString bool " / " expr1 expr2
exprToString bool (Or expr1 expr2) = genBinopString bool " || " expr1 expr2
exprToString bool (And expr1 expr2) = genBinopString bool " && " expr1 expr2
exprToString bool (Equal expr1 expr2) = genBinopString bool " = " expr1 expr2
exprToString bool (NotEqual expr1 expr2) = genBinopString bool " != " expr1 expr2
exprToString bool (Less expr1 expr2) = genBinopString bool " < " expr1 expr2
exprToString bool (LessEqual expr1 expr2) = genBinopString bool " <= " expr1 expr2
exprToString bool (Greater expr1 expr2) = genBinopString bool " > " expr1 expr2
exprToString bool (GreaterEqual expr1 expr2) = genBinopString bool " >= " expr1 expr2
exprToString _ (Neg expr) = "!" ++ (exprToString True expr)
exprToString _ (UMinus expr) = "-" ++ (exprToString True expr)
