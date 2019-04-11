import GoatAST

printProgram :: GoatProgram -> String

printProgram Program [] =
  return ()

printProgram Program (p:procedures) =
  do
    printProcedure p
    -- check whether procedures list is empty, to determine whether there should be a new line
    putStrLn ""
    printProgram procedures


printProcedure :: Procedure -> IO()
printProcedure Procedure header decl stmt =
  do
    printHeader header
    printDeclarations decl
    printStatements stmt

printHeader :: Header -> IO()
printHeader Header ident parameters =
  do
    putstr ("proc " ++ ident ++ " (")
    printParameters parameters
    putStrLn ")"


printParameters :: [Parameter] -> IO()
printParameters [] =
  return ()
printParameters (p:parameters) =
  do
    printParameter p
    if length(parameters) > 0
      then putstr ", "
      else return ()

printParameter :: Parameter -> IO()
printParameter Parameter indicator basetype ident =
  do
    putStr (Show indicator)
    putStr " "
    putStr (Show basetype)
    putStr (" " ++ ident)

printDeclarations :: [Decl] -> IO()
printDeclarations [] = return ()
printDeclarations (decl:decls) =
  do
    printDeclaration decl
    printDeclarations decls

printDeclaration :: Decl -> IO()
printDeclaration Decl baseType var =
  do
    putStr "    "
    printBaseType baseType
    putStr " "
    printVar var
    putStrLn ";"

printBaseType :: BaseType -> IO()
printBaseType baseType =
  |baseType == BoolType    =putStr "bool"
  |baseType == IntType     =putStr "Int"
  |baseType == FloatType   =putStr "float"

printVar :: Var -> IO()
printVar BaseVar ident =
  putStr ident
printVar ShapeVar ident shape =
  do
    putStr ident
    printShape shape
printVar IndexVar ident index =
  do
    putStr ident
    printIndex index

printShape :: Shape -> IO()
printShape SShape a =
  putStr ("[" ++ show(int) ++ "]")
printShape DShape a b =
  do
    printShape SShape a
    printShape SShape b

printIndex :: Index -> IO()
printIndex SIndex expr =
  do
    putStr "["
    printExpr expr
    putStr "]"
printIndex DIndex expr1 expr2 =
  do
    printIndex (SIndex expr1)
    printIndex (SIndex expr2)

printExpr :: Expr -> IO()
printExpr expr = return ()


printStatements :: [Statement] -> IO()



printStatement :: Statment -> IO()
printStatement (While expr stmts) =
  do
    putStr "while "
    printExpr expr
    putStrLn " do"
    map printStatement stmts
