module GoatCompiler where

import Control.Monad.State
import Data.Map (
  Map,
  (!),
  )
import qualified Data.Map as Map
import GoatAST
import GoatFormat (stmtVarToStr, exprToStr, exprsToStr)
import Text.Parsec.Pos

data VarShape
  = Single
  | Array Int
  | Matrix Int Int
  deriving (Show, Eq)

data SymTable = SymTable
  { labelCounter :: Int
  , slotCounter  :: Int
  , regCounter   :: Int
  , code         :: String
  , variables    :: Map String (Bool, BaseType, VarShape, Int)
  , procedures   :: Map String ([(Bool, BaseType)])
  } deriving (Show)

-----------SymTable Helper-----------

-- Return the next available slot number
-- Slot counter increases in symbol table
nextAvailableSlot :: State SymTable Int
nextAvailableSlot
  = do
      st <- get
      let slot = slotCounter st
      put $ st {slotCounter = (slot + 1)}
      return slot

-- Return the next available register number
-- Register counter increases in symbol table
nextAvailableReg :: State SymTable Int
nextAvailableReg
  = do
      st <- get
      let reg = regCounter st
      -- Check register number exceeds 1024
      if reg > 1023
        then error $ "number of register exceeds 1024"
        else
          do
            put $ st {regCounter = (reg + 1)}
            return reg

-- Return the next available label number
-- Label counter increases in symbol table
nextAvailableLabel :: State SymTable Int
nextAvailableLabel
  = do
      st <- get
      let label = labelCounter st
      put $ st {labelCounter = (label + 1)}
      return label

-- Insert procedure into symbol table
insertProcedure :: String -> [(Bool, BaseType)] -> SourcePos -> State SymTable ()
insertProcedure ident types pos
  = do
      st <- get
      if Map.member ident (procedures st)
          -- Check if duplicated procedure exists
          then error $ putPosition pos ++ "duplicated procedure " ++ ident
          else put $ st { procedures = Map.insert ident types (procedures st) }

-- Insert variable into symbol table
insertVariable :: String -> (Bool, BaseType, VarShape, Int) -> SourcePos -> State SymTable ()
insertVariable ident (isVal, bt, vs, slot) pos
  = do
      st <- get
      if Map.member ident (variables st)
          -- Check if duplicated variable exists
          then error $ putPosition pos ++ "duplicate variable " ++ ident
          else put $ st { variables = Map.insert ident (isVal, bt, vs, slot) (variables st) }

-- Reset slot counter in symbol table
resetSlot :: State SymTable ()
resetSlot
  = do
      st <- get
      put $ st { slotCounter = 0 }

-- Reset register counter in symbol table
resetReg :: State SymTable ()
resetReg
  = do
      st <- get
      put $ st { regCounter = 0 }

-- Reset variable map in symbol table
resetVariable :: State SymTable ()
resetVariable
  = do
      st <- get
      put $ st { variables = Map.empty }

-- Reset procedure map in symbol table
resetProcedure :: State SymTable ()
resetProcedure
  = do
      resetReg
      resetSlot
      resetVariable

-- Return the variable with given identifier
getVariable :: String -> VarShape -> SourcePos -> State SymTable (Bool, BaseType, VarShape, Int)
getVariable ident vs pos
  = do
      st <- get
      case Map.lookup ident (variables st) of
          -- Variable of given identifier is undefined
          Nothing -> error $ putPosition pos ++ "undefined variable " ++ ident
          Just (isVal, baseType, varShape, slot)
            -> if sameVarShapeType vs varShape
                then return (isVal, baseType, varShape, slot)
                else error $ putPosition pos ++ "type of variable is not matching " ++ ident

-- Check given var shapes are the same type
sameVarShapeType :: VarShape -> VarShape -> Bool
sameVarShapeType Single Single = True
sameVarShapeType (Array _) (Array _) = True
sameVarShapeType (Matrix _ _) (Matrix _ _) = True
sameVarShapeType _ _ = False

-- Return the procedure with given identifier
getProcedure :: String -> SourcePos -> State SymTable ([(Bool, BaseType)])
getProcedure ident pos
  = do
      st <- get
      case Map.lookup ident (procedures st) of
        Nothing -> error $ putPosition pos ++ "undefined procedure " ++ ident
        Just v -> return v

-- Return the parameter with given procedure identifier and index
getProcParameter :: String -> Int -> State SymTable (Bool, BaseType)
getProcParameter ident idx
  = do
      st <- get
      return $ (procedures st) ! ident !! idx

-----------Main Compiler-----------

-- Compile Goat program
compileProg :: GoatProg -> State SymTable ()
compileProg (Prog ps)
  = do
      -- Call main procedure
      putCode $ "    call proc_main\n"
      -- Stop the program
      putCode $ "    halt\n"
      -- Put procedures into map
      putProcedures ps
      -- Check main procedure exists
      checkMain
      -- Compile all procedures
      compileProcedures ps

-----------Procedure Compiler-----------

-- Compile procedures (recursion)
compileProcedures :: [Proc] -> State SymTable ()
compileProcedures [] = return ()
compileProcedures (p:ps)
  = do
      compileProcedure p
      compileProcedures ps

-- Compile procedure in detail
compileProcedure :: Proc -> State SymTable ()
compileProcedure (Proc _ ident params decls stmts)
  = do
      -- Put label
      putProcedureLabel ident
      -- Put prologue
      stackSize <- putProcedurePrologue params decls
      -- Put parameter passing comment
      unless (null params) $ putComment "parameter passing"
      -- Put paramaters
      putParameters params
      -- Reset register
      resetReg
      -- Initialise declarations
      putDeclarations decls
      -- Reset register
      resetReg
      -- Put statements
      putStatements stmts
      -- Reset register
      resetReg
      -- Put epilogue
      putProcedureEpilogue stackSize
      -- Reset procedure
      resetProcedure
      return ()

----------------------Helper----------------------

-- Put given string into code string
putCode :: String -> State SymTable ()
putCode s
  = do
      st <- get
      let c = code st
      put $ st { code = c ++ s }

-- Put given comment into code string
putComment :: String -> State SymTable ()
putComment cmt
  = do
      putCode $ "# " ++ cmt ++ "\n"

-- Return position string from SourcePos
putPosition :: SourcePos -> String
putPosition pos
  =
    let
      line = sourceLine pos
      column = sourceColumn pos
    in
      "Line " ++ show line ++ " Column " ++ show column ++ "\n"


----------- Statement Helper -----------

-- Put statement label into code string
putStmtLabel :: Int -> State SymTable ()
putStmtLabel a
  = do
      putCode ("label_" ++ show a ++ ":\n")

-- Put statements (recursion)
putStatements :: [Stmt] -> State SymTable ()
putStatements [] = return ()
putStatements (stmt:stmts)
  = do
      putStatement stmt
      putStatements stmts

-- Put statement into code string
putStatement :: Stmt -> State SymTable ()
putStatement stmt
    = do
        compileStmt stmt
        -- Reset register
        resetReg

-- Put statement comment into code string
putStmtComment :: Stmt -> State SymTable ()
putStmtComment stmt
  = do
      putCode $ "# " ++ compileStmtComment stmt ++ "\n"

-- Compile statement comment based on statement type
compileStmtComment :: Stmt -> String
compileStmtComment (Assign _ stmtVar expr)
  = stmtVarToStr stmtVar ++ " := " ++ exprToStr False expr ++ ";"
compileStmtComment (Read _ stmtVar) = "read " ++ stmtVarToStr stmtVar ++ ";"
compileStmtComment (Write _ expr) = "write " ++ exprToStr False expr ++ ";"
compileStmtComment (SWrite _ string) = "write \"" ++ string ++ "\";"
compileStmtComment (Call _ ident exprs)
  = "call " ++ ident ++ "(" ++ exprsToStr exprs ++ ");"
compileStmtComment (If _ expr _ _) = "if " ++ exprToStr False expr
compileStmtComment (While _ expr _) = "while " ++ exprToStr False expr

-- Compile statements (recursion)
compileStmts :: [Stmt] -> State SymTable ()
compileStmts [] 
  = do
      resetReg 
      return ()
compileStmts (stmt:stmts)
  = do
      resetReg
      compileStmt stmt
      compileStmts stmts
---------------------------------------------------------------------------------------------------------------------

-- Return the base type of given statement variable
getStmtVarBaseType :: StmtVar -> SourcePos -> State SymTable BaseType
-- Single variable case
getStmtVarBaseType (SBaseVar ident) pos
  = do
      (_, baseType, _, _) <- getVariable ident (Single) pos
      return baseType
-- Array or Matrix variable case
getStmtVarBaseType (IndexVar ident index) pos
  = do
      (_, baseType, _, _) <- getVariable ident (convertIndex2VarShape index) pos
      return baseType

-- Convert Index type into VarShape type
convertIndex2VarShape :: Index -> VarShape
convertIndex2VarShape (IArray _) = (Array 0)
convertIndex2VarShape (IMatrix _ _) = (Matrix 0 0)

-- Check if two types can be assigned
-- Note: Float can be assigned by Int
assignableType :: BaseType -> BaseType -> Int -> State SymTable Bool
assignableType FloatType IntType reg
  = do
      putCode $ "    int_to_real r" ++ show reg ++ ", r" ++ show reg ++ "\n"
      return True
assignableType st ex reg
  = do 
      return (st == ex)

-- Put assignment code
putAssignCode :: StmtVar -> Int -> SourcePos -> State SymTable ()
-- Variable case
putAssignCode (SBaseVar ident) reg pos
  = do
      (isVal, _, _, slot) <- getVariable ident (Single) pos
      if not isVal then
        -- Ref indicator
        putAssignCodeRef slot reg
      else
        -- Val indicator
        putCode $ "    store " ++ show slot ++ ", r" ++ show reg ++ "\n"
-- Array case
putAssignCode (IndexVar ident (IArray expr)) reg pos
  = do
      (_, _, _, slot) <- getVariable ident (Array 0) pos
      offsetReg <- nextAvailableReg
      exprType <- compileExpr offsetReg expr
      -- Check if index is Int type
      if exprType == IntType then
        putAssignCodeOffset offsetReg slot reg
      else
        error $ putPosition pos ++ "array index is not Int"
-- Matrix case
putAssignCode (IndexVar ident (IMatrix expr1 expr2)) reg pos
  = do
      (_, _, (Matrix row col), slot) <- getVariable ident (Matrix 0 0) pos
      offsetReg <- nextAvailableReg
      colReg <- nextAvailableReg
      expr1Type <- compileExpr offsetReg expr1
      expr2Type <- compileExpr colReg expr2
      -- Check if indexes are Int type
      if expr1Type == IntType && expr2Type == IntType then
        do
          putSetOffsetReg offsetReg colReg col
          putAssignCodeOffset offsetReg slot reg
      else
        error $ putPosition pos ++ "matrix index is not Int"

-- Put calculation of offset register for Matrix
putSetOffsetReg :: Int -> Int -> Int -> State SymTable ()
putSetOffsetReg offsetReg colReg col
  = do
      sizeReg <- nextAvailableReg
      putCode $ "    int_const r" ++ show sizeReg ++ ", " ++ show col ++ "\n"
      putCode $ "    mul_int r" ++ show offsetReg ++ ", r" ++ show offsetReg ++ ", r" ++ show sizeReg ++ "\n"
      putCode $ "    add_int r" ++ show offsetReg ++ ", r" ++ show offsetReg ++ ", r" ++ show colReg ++ "\n"

-- Put calculation of stack array's index
putAssignCodeOffset :: Int -> Int -> Int -> State SymTable ()
putAssignCodeOffset offsetReg startSlot reg
  = do
      addrReg <- nextAvailableReg
      putCode $ "    load_address r" ++ show addrReg ++ ", " ++ show startSlot ++ "\n"
      putCode $ "    sub_offset r" ++ show addrReg ++ ", r" ++ show addrReg ++ ", r" ++ show offsetReg ++ "\n"
      putCode $ "    store_indirect r" ++ show addrReg ++ ", r" ++ show reg ++ "\n"

-- Put assignment code for Ref Indicator
putAssignCodeRef :: Int -> Int -> State SymTable ()
putAssignCodeRef addrSlot reg
  = do
      addrReg <- nextAvailableReg
      putCode $ "    load r" ++ show addrReg ++ ", " ++ show addrSlot ++ "\n"
      putCode $ "    store_indirect r" ++ show addrReg ++ ", r" ++ show reg ++ "\n"

-- Put read instruction based on base type of variable
putReadCodeType :: BaseType -> State SymTable ()
putReadCodeType baseType
  = do
      putCode ("    call_builtin ")
      case baseType of
        BoolType   -> do
                        putCode ("read_bool\n")
        IntType    -> do
                        putCode ("read_int\n")
        FloatType  -> do
                        putCode ("read_real\n")

---------------------------------------------------------------------------------------------------------------------
-- Compile statement
compileStmt :: Stmt -> State SymTable ()
-- Assign statement
compileStmt (Assign pos stmtVar expr)
  = do
      regThis <- nextAvailableReg
      putStmtComment (Assign pos stmtVar expr)
      exprType <- compileExpr regThis expr
      stmtType <- getStmtVarBaseType stmtVar pos
      assignable <- assignableType stmtType exprType regThis
      if assignable then
        putAssignCode stmtVar regThis pos
      else
        error $ putPosition pos ++ "assginment type dose not match"

-- Read statement
compileStmt (Read pos stmtVar)
  = do
      reg <- nextAvailableReg
      baseType <- getStmtVarBaseType stmtVar pos
      putStmtComment (Read pos stmtVar)
      putReadCodeType baseType
      putAssignCode stmtVar reg pos

-- Write statement
compileStmt (Write pos expr)
  = do
      reg <- nextAvailableReg
      putStmtComment (Write pos expr)
      baseType <- compileExpr reg expr
      let func = case baseType of
                      BoolType -> "print_bool"
                      IntType -> "print_int"
                      FloatType -> "print_real"
      putCode ("    call_builtin " ++ func ++ "\n")

-- Write string statement
compileStmt (SWrite pos string)
  = do
      reg <- nextAvailableReg
      putStmtComment (SWrite pos string)
      putCode ("    string_const r" ++ show reg ++ ", \"" ++ string ++ "\"" ++ "\n")
      putCode ("    call_builtin print_string" ++ "\n")

-- Call statement
compileStmt (Call pos ident es)
  = do
      reg <- nextAvailableReg
      proc <- getProcedure ident pos
      if (length proc) == (length es) then
        do
          putStmtComment (Call pos ident es)
          compileExprs ident reg es
          putCode ("    call proc_" ++ ident ++ "\n")
      else error $ putPosition pos ++ "procedure call arity dose not matched \n"

-- If-then statement
compileStmt (If pos expr stmts [])
  = do
      reg <- nextAvailableReg
      afterThen <- nextAvailableLabel
      putStmtComment (If pos expr stmts [])
      exprType <- compileExpr reg expr
      if exprType == BoolType then
        do
          putCode ("    branch_on_false r0, label_" ++ show(afterThen) ++ "\n")
          putCode ("# then\n")
          compileStmts stmts
          putCode ("# fi\n")
          putStmtLabel afterThen
      else
        error $ putPosition pos ++ "Expression of If statement can not have type " ++ show(exprType)

-- If-then-else statement
compileStmt (If pos expr thenStmts elseStmts)
  = do
      reg <- nextAvailableReg
      inElse <- nextAvailableLabel
      afterElse <- nextAvailableLabel
      putStmtComment (If pos expr thenStmts elseStmts)
      exprType <- compileExpr reg expr
      if exprType == BoolType then
        do
          putCode ("    branch_on_false r0, label_" ++ show(inElse) ++ "\n")
          putCode ("# then\n")
          compileStmts thenStmts
          putCode ("    branch_uncond label_" ++ show(afterElse) ++ "\n")
          putStmtLabel inElse
          putCode ("# else\n")
          compileStmts elseStmts
          putCode ("# fi\n")
          putStmtLabel afterElse
      else
        error $ putPosition pos ++ "Expression of If statement can not have type " ++ show(exprType)

-- While statement
compileStmt (While pos expr stmts)
  = do
      reg <- nextAvailableReg
      inWhile <- nextAvailableLabel
      afterWhile <- nextAvailableLabel
      putStmtComment (While pos expr stmts)
      putStmtLabel inWhile
      exprType <- compileExpr reg expr
      if exprType == BoolType then
        do
          putCode ("    branch_on_false r0, label_" ++ show afterWhile ++ "\n")
          putCode ("# do\n")
          compileStmts stmts
          putCode ("    branch_uncond label_" ++ show inWhile ++ "\n")
          putCode ("# od\n")
          putStmtLabel afterWhile
      else
        error $ putPosition pos ++ "Expression of While statement can not have type " ++ show(exprType)

compileExprs :: String -> Int -> [Expr] -> State SymTable ()
compileExprs _ _ [] = return ()
compileExprs ident n (e:es)
  = do
      let pos = getExprPos e
      (isVal, baseType) <- getProcParameter ident n
      n1 <- nextAvailableReg
      if isVal then
        do
          exprType <- compileExpr n e
          if exprType == baseType then
            compileExprs ident n1 es
          else
            if exprType == IntType && baseType == FloatType
              then 
                do
                  putCode $ "    int_to_real r" ++ show n ++ ", r" ++ show n ++ "\n"
                  compileExprs ident n1 es
              else error $ putPosition pos ++ " procedure parameter dose not match "
      else
        case e of
          (Id _ stmtVar)
            -> do
                  stmtVarType <- getStmtVarBaseType stmtVar pos
                  if stmtVarType == baseType then
                    do
                      storeAddressToRegN stmtVar n pos
                      compileExprs ident n1 es
                  else
                    error $ putPosition pos ++ " procedure parameter dose not match "
          _ -> error $ putPosition pos ++ " Ref procedure parameter dose not allow Non-lvalue "

getExprPos :: Expr -> SourcePos
getExprPos (Id pos _) = pos
getExprPos (BoolConst pos _) = pos
getExprPos (IntConst pos _) = pos
getExprPos (FloatConst pos _) = pos
getExprPos (Add pos _ _) = pos
getExprPos (Minus pos _ _) = pos
getExprPos (Mul pos _ _) = pos
getExprPos (Div pos _ _) = pos
getExprPos (Or pos _ _) = pos
getExprPos (And pos _ _) = pos
getExprPos (Equal pos _ _) = pos
getExprPos (NotEqual pos _ _) = pos
getExprPos (Less pos _ _) = pos
getExprPos (LessEqual pos _ _) = pos
getExprPos (Greater pos _ _) = pos
getExprPos (GreaterEqual pos _ _) = pos
getExprPos (Neg pos _) = pos
getExprPos (UMinus pos _) = pos

storeAddressToRegN :: StmtVar -> Int -> SourcePos -> State SymTable ()
storeAddressToRegN (SBaseVar ident) destReg pos
  = do
      (isVal, baseType, varShape, slot) <- getVariable ident (Single) pos
      if not isVal then
        putCode $ "    load r" ++ show destReg ++ ", " ++ show slot ++ "\n"
      else
        putCode $ "    load_address r" ++ show destReg ++ ", " ++ show slot ++ "\n"

storeAddressToRegN (IndexVar ident (IArray expr)) destReg pos
  = do
      (isVal, baseType, varShape, slot) <- getVariable ident (Array 0) pos
      offsetReg <- nextAvailableReg
      exprType <- compileExpr offsetReg expr
      if exprType == IntType then
        putStoreAddressCodeOffset offsetReg slot destReg
      else
        error $ putPosition pos ++ " array index is not Int " ++ ident

storeAddressToRegN (IndexVar ident (IMatrix expr1 expr2)) destReg pos
  = do
      (isVal, baseType, (Matrix row col), slot) <- getVariable ident (Matrix 0 0) pos
      offsetReg <- nextAvailableReg
      colReg <- nextAvailableReg
      expr1Type <- compileExpr offsetReg expr1
      expr2Type <- compileExpr colReg expr2
      if expr1Type == IntType && expr2Type == IntType then
        do
          putSetOffsetReg offsetReg colReg col
          putStoreAddressCodeOffset offsetReg slot destReg
      else
        error $ putPosition pos ++ " matrix index is not Int " ++ ident

putStoreAddressCodeOffset :: Int -> Int -> Int -> State SymTable ()
putStoreAddressCodeOffset offsetReg startSlot destReg
  = do
      putCode $ "    load_address r" ++ show destReg ++ ", " ++ show startSlot ++ "\n"
      putCode $ "    sub_offset r" ++ show destReg ++ ", r" ++ show destReg ++ ", r" ++ show offsetReg ++ "\n"

----------- Declartion Helper -----------

putDeclarations :: [Decl] -> State SymTable ()
putDeclarations [] = return ()
putDeclarations ds
  = do
      ri <- nextAvailableReg
      putCode $ "    int_const r" ++ show ri ++ ", 0\n"
      rf <- nextAvailableReg
      putCode $ "    real_const r" ++ show rf ++ ", 0.0\n"
      putDeclarations' ri rf ds

putDeclarations' :: Int -> Int -> [Decl] -> State SymTable ()
putDeclarations' _ _ [] = return ()
putDeclarations' ri rf (d:ds)
  = do
      let r = whichDeclReg ri rf d
      putDeclaration' r d
      putDeclarations' ri rf ds

putDeclaration' :: Int -> Decl -> State SymTable ()
putDeclaration' r (Decl pos baseType declVar)
  = do
      case declVar of
        (DBaseVar ident)
          -> do
                putDeclComment (DBaseVar ident) baseType
                slot <- nextAvailableSlot
                insertVariable ident (True, baseType, Single, slot) pos
                putCode $ "    store " ++ show slot ++ ", r" ++ show r ++ "\n"
        (ShapeVar ident shape)
          -> case shape of
                  (SArray num)
                    -> do
                          putDeclComment (ShapeVar ident (SArray num)) baseType
                          slot <- nextAvailableSlot
                          insertVariable ident (True, baseType, (Array num), slot) pos
                          putCode $ "    store " ++ show slot ++ ", r" ++ show r ++ "\n"
                          putFilledSkipSlot r (num - 1)
                  (SMatrix row col)
                    -> do
                          putDeclComment (ShapeVar ident (shape)) baseType
                          slot <- nextAvailableSlot
                          insertVariable ident (True, baseType, (Matrix row col), slot) pos
                          putCode $ "    store " ++ show slot ++ ", r" ++ show r ++ "\n"
                          putFilledSkipSlot r (row * col - 1)

putFilledSkipSlot :: Int -> Int -> State SymTable ()
putFilledSkipSlot _ 0 = return ()
putFilledSkipSlot r num
  = do
      slot <- nextAvailableSlot
      putCode $ "    store " ++ show slot ++ ", r" ++ show r ++ "\n"
      putFilledSkipSlot r (num - 1)


putDeclComment :: DeclVar -> BaseType -> State SymTable ()
putDeclComment declVar baseType
  = do
      let tStr = case baseType of
                      IntType -> "int"
                      BoolType -> "bool"
                      FloatType -> "float"
      let iStr = case declVar of
                      (DBaseVar ident) -> ident
                      (ShapeVar ident (SArray num)) -> ident ++ "[" ++ show num ++ "]"
                      (ShapeVar ident (SMatrix row col)) -> ident ++ "[" ++ show row ++ "," ++ show col ++ "]"
      putCode $ "# initialise " ++ tStr ++ " val " ++ iStr ++ "\n"
      return ()

whichDeclReg :: Int -> Int -> Decl -> Int
whichDeclReg ri rf (Decl _ baseType _) = if baseType == FloatType then rf else ri

----------- Expression Helper -----------

compileExpr :: Int -> Expr -> State SymTable BaseType
compileExpr reg (BoolConst _ bool)
  = do
      let boolInt = if bool then 1 else 0
      putCode ("    int_const r" ++ show reg ++ ", " ++ show boolInt ++ "\n")
      return BoolType

compileExpr reg (IntConst _ i)
  = do
      putCode ("    int_const r" ++ show reg ++ ", " ++ show i ++ "\n")
      return IntType

compileExpr reg (FloatConst _ f)
  = do
      putCode ("    real_const r" ++ show reg ++ ", " ++ show f ++ "\n")
      return FloatType

compileExpr reg (Add pos expr1 expr2)          = compileArithmetricExpr "add" reg expr1 expr2 pos
compileExpr reg (Minus pos expr1 expr2)        = compileArithmetricExpr "sub" reg expr1 expr2 pos
compileExpr reg (Mul pos expr1 expr2)          = compileArithmetricExpr "mul" reg expr1 expr2 pos
compileExpr reg (Div pos expr1 expr2)          = compileArithmetricExpr "div" reg expr1 expr2 pos
compileExpr reg (Equal pos expr1 expr2)        = compileEqualityExpr "eq" reg expr1 expr2 pos
compileExpr reg (NotEqual pos expr1 expr2)     = compileEqualityExpr "ne" reg expr1 expr2 pos
compileExpr reg (Or pos expr1 expr2)           = compileLogicalExpr "or" "true" reg expr1 expr2 pos
compileExpr reg (And pos expr1 expr2)          = compileLogicalExpr "and" "false" reg expr1 expr2 pos
compileExpr reg (Less pos expr1 expr2)         = compileCompareExpr "lt" reg expr1 expr2 pos
compileExpr reg (LessEqual pos expr1 expr2)    = compileCompareExpr "le" reg expr1 expr2 pos
compileExpr reg (Greater pos expr1 expr2)      = compileCompareExpr "gt" reg expr1 expr2 pos
compileExpr reg (GreaterEqual pos expr1 expr2) = compileCompareExpr "ge" reg expr1 expr2 pos

compileExpr reg (Neg pos expr)
  = do
      type1 <- compileExpr reg expr
      if type1 == BoolType then
        do
          putCode ("    not r" ++ show reg ++ ", r" ++ show reg ++ "\n")
          return BoolType
      else
        error $ putPosition pos ++ "Can not negate type " ++ show type1

compileExpr reg (UMinus pos expr)
  = do
      type1 <- compileExpr reg expr
      -- if type1 == IntType || type1 == FloatType then
      --   do
      --     putCode ("    not r" ++ show reg ++ ", r" ++ show reg ++ "\n")
      --     return type1
      -- else
      --   error $ putPosition pos ++ "Can not negate type " ++ show type1
      case type1 of
        IntType
          -> do
                putCode ("    neg_int r" ++ show reg ++ ", r" ++ show reg ++ "\n")
                return type1
        FloatType
          -> do
                putCode ("    neg_real r" ++ show reg ++ ", r" ++ show reg ++ "\n")
                return type1
        _
          -> error $ putPosition pos ++ "Can not negate type " ++ show type1

compileExpr reg (Id pos (SBaseVar ident))
  = do
      (isVal, baseType, varShape, slotnum) <- getVariable ident (Single) pos
      if varShape == Single then
        if isVal then
          do
            putCode ("    load r" ++ show reg ++ ", " ++ show slotnum ++ "\n")
            return baseType
        else
          do
            putCode ("    load r" ++ show reg ++ ", " ++ show slotnum ++ "\n")
            putCode ("    load_indirect r" ++ show reg ++ ", r" ++ show reg ++ "\n")
            return baseType
      else
        error $ putPosition pos ++ "Expected type " ++ show varShape ++ ", while type Single received"

compileExpr reg (Id pos (IndexVar ident (IArray expr)))
  = do
      (_, baseType, varShape, slotnum) <- getVariable ident (Array 0) pos
      reg1 <- nextAvailableReg
      exprType <- compileExpr reg1 expr
      case varShape of
        (Array n)
          -> do
                if exprType == IntType then
                  do
                    putCode ("    load_address r" ++ show reg ++ ", " ++ show slotnum ++ "\n")
                    putCode ("    sub_offset r" ++ show reg ++ ", r" ++ show reg ++ ", r" ++ show reg1 ++ "\n")
                    putCode ("    load_indirect r" ++ show reg ++ ", r" ++ show reg ++ "\n")
                    return baseType
                else
                  error $ putPosition pos ++ "Array Index must be IntType, while type " ++ show exprType ++ " received"
        Single
          -> error $ putPosition pos ++ "Expect Single expression, while Array expression is given"
        (Matrix _ _)
          -> error $ putPosition pos ++ "Expect Matrix expression, while Array expression is given"

compileExpr reg (Id pos (IndexVar ident (IMatrix expr1 expr2)))
  = do
      (_, baseType, varShape, slotnum) <- getVariable ident (Matrix 0 0) pos
      reg1 <- nextAvailableReg
      reg2 <- nextAvailableReg
      type1 <- compileExpr reg1 expr1
      type2 <- compileExpr reg2 expr2
      case varShape of
        (Matrix a b)
          -> do
                if type1 == IntType && type2 == IntType then
                  do
                    reg3 <- nextAvailableReg
                    putCode $ "    int_const r" ++ show reg3 ++ ", " ++ show b ++ "\n"
                    putCode $ "    mul_int r" ++ show reg3 ++ ", r" ++ show reg3 ++ ", r" ++ show reg1 ++ "\n"
                    putCode $ "    add_int r" ++ show reg3 ++ ", r" ++ show reg3 ++ ", r" ++ show reg2 ++ "\n"
                    putCode $ "    load_address r" ++ show reg ++ ", " ++ show slotnum ++ "\n"
                    putCode $ "    sub_offset r" ++ show reg ++ ", r" ++ show reg ++ ", r" ++ show reg3 ++ "\n"
                    putCode $ "    load_indirect r" ++ show reg ++ ", r" ++ show reg ++ "\n"
                    return baseType
                else
                  error $ putPosition pos ++ "Matrix Index must be IntType, while type " ++ show type1 ++ " and type " ++ show type2 ++ " received"
        (Single)
          -> error $ putPosition pos ++ "Expect Single expression, while Matrix expression is given"
        (Array _)
          -> error $ putPosition pos ++ "Expect Array expression, while Matrix expression is given"

compileArithmetricExpr :: String -> Int -> Expr -> Expr -> SourcePos -> State SymTable BaseType
compileArithmetricExpr s reg expr1 expr2 pos
  = do
      reg1 <- nextAvailableReg
      type1 <- compileExpr reg expr1
      type2 <- compileExpr reg1 expr2
      if type1 == type2 then
        if type1 == IntType then
          do
            putCode ("    " ++ s ++ "_int r" ++ show reg ++ ", r" ++ show reg ++ ", r" ++ show reg1 ++ "\n")
            return IntType
        else
          if type1 == FloatType then
            do
              putCode ("    " ++ s ++ "_real r" ++ show reg ++ ", r" ++ show reg ++ ", r" ++ show reg1 ++ "\n")
              return FloatType
          else
            error $ putPosition pos ++ "Can not " ++ s ++ " type " ++ show type1 ++ " with type " ++ show type2
      else
        if type1 == IntType && type2 == FloatType then
          do
            putCode ("    int_to_real r" ++ show reg ++ ", r" ++ show reg ++ "\n")
            putCode ("    " ++ s ++ "_real r" ++ show reg ++ ", r" ++ show reg ++ ", r" ++ show reg1 ++ "\n")
            return FloatType
        else
          if type1 == FloatType && type2 == IntType then
            do
              putCode ("    int_to_real r" ++ show reg1 ++ ", r" ++ show reg1 ++ "\n")
              putCode ("    " ++ s ++ "_real r" ++ show reg ++ ", r" ++ show reg ++ ", r" ++ show reg1 ++ "\n")
              return FloatType
          else
            error $ putPosition pos ++ "Can not " ++ s ++ " type " ++ show type1 ++ " with type " ++ show type2

compileEqualityExpr :: String -> Int -> Expr -> Expr -> SourcePos -> State SymTable BaseType
compileEqualityExpr s reg expr1 expr2 pos
  = do
      reg1 <- nextAvailableReg
      type1 <- compileExpr reg expr1
      type2 <- compileExpr reg1 expr2
      if type1 == type2 then
        if type1 == FloatType then
          do
            putCode ("    cmp_" ++ s ++ "_real" ++ " r" ++ show reg ++ ", r" ++ show reg ++ ", r" ++ show reg1 ++ "\n")
            return BoolType
        else
          do
            putCode ("    cmp_" ++ s ++ "_int" ++ " r" ++ show reg ++ ", r" ++ show reg ++ ", r" ++ show reg1 ++ "\n")
            return BoolType
      else
        error $ putPosition pos ++ "Can not compare " ++ s ++ " with type " ++ show type1 ++ " and type " ++ show type2

compileLogicalExpr :: String -> String -> Int -> Expr -> Expr -> SourcePos -> State SymTable BaseType
compileLogicalExpr s boolstr reg expr1 expr2 pos
  = do
      reg1 <- nextAvailableReg
      after <- nextAvailableLabel
      type1 <- compileExpr reg expr1
      putCode ("    branch_on_" ++ boolstr ++ " r" ++ show reg ++ ", label_" ++ show after ++ "\n")
      type2 <- compileExpr reg1 expr2
      putCode ("    " ++ s ++ " r" ++ show reg ++ ", r" ++ show reg ++ ", r" ++ show reg1 ++ "\n")
      putStmtLabel after
      if type1 == BoolType && type2 == BoolType then
        return BoolType
      else
        error $ putPosition pos ++ s ++ " operation can not be used between type " ++ show type1 ++ " and " ++ show type2

compileCompareExpr :: String -> Int -> Expr -> Expr -> SourcePos -> State SymTable BaseType
compileCompareExpr s reg expr1 expr2 pos
  = do
      reg1 <- nextAvailableReg
      type1 <- compileExpr reg expr1
      type2 <- compileExpr reg1 expr2
      if type1 == type2 then
        if type1 == FloatType then
          do
            putCode ("    cmp_" ++ s ++ "_real r" ++ show reg ++ ", r" ++ show reg ++ ", r" ++ show reg1 ++ "\n")
            return BoolType
        else
          do
            putCode ("    cmp_" ++ s ++ "_int r" ++ show reg ++ ", r" ++ show reg ++ ", r" ++ show reg1 ++ "\n")
            return BoolType
      else
        if type1 == IntType && type2 == FloatType then
          do
              putCode ("    int_to_real r" ++ show reg ++ ", r" ++ show reg ++ "\n")
              putCode ("    cmp_" ++ s ++ "_real r" ++ show reg ++ ", r" ++ show reg ++ ", r" ++ show reg1 ++ "\n")
              return BoolType
        else
          if type1 == FloatType && type2 == IntType then
            do
              putCode ("    int_to_real r" ++ show reg1 ++ ", r" ++ show reg1 ++ "\n")
              putCode ("    cmp_" ++ s ++ "_real r" ++ show reg ++ ", r" ++ show reg ++ ", r" ++ show reg1 ++ "\n")
              return BoolType
          else
              error $ putPosition pos ++ "Can not compare" ++ s ++ " with type " ++ show type1 ++ " and type " ++ show type2

----------- Parameters Helper -----------

-- Put parameters (recursion)
putParameters :: [Param] -> State SymTable ()
putParameters [] = return ()
putParameters (param:params)
  = do
      putParameter param
      putParameters params

-- Put parameter
putParameter :: Param -> State SymTable ()
putParameter (Param pos indicator baseType ident)
  = do
      slot <- nextAvailableSlot
      reg <- nextAvailableReg
      insertVariable ident ((indicator == Val), baseType, Single, slot) pos
      putCode $ "    store " ++ show slot ++ ", r" ++ show reg ++ "\n"

------------------ Procedure Helper ----------------------

-- Put procedure label
putProcedureLabel :: String -> State SymTable ()
putProcedureLabel ident
  = do
      putCode $ "proc_" ++ ident ++ ":\n"

-- Put procedure prologue
putProcedurePrologue :: [Param] -> [Decl] -> State SymTable Int
putProcedurePrologue params decls
  = do
      putComment "prologue"
      let ps = length params
      let ds = getDeclsSize decls
      putCode $ "    push_stack_frame " ++ show (ps + ds) ++ "\n"
      return (ps + ds)

-- Put procedure epilogue
putProcedureEpilogue :: Int -> State SymTable ()
putProcedureEpilogue n
  = do
      putComment "epilogue"
      putCode $ "    pop_stack_frame " ++ show n ++ "\n"
      putCode $ "    return\n"
      return ()

-- Return the size of declarations (recursion)
getDeclsSize :: [Decl] -> Int
getDeclsSize [] = 0
getDeclsSize (d:ds) = (getDeclSize d) + (getDeclsSize ds)

-- Return the size of declaration
getDeclSize :: Decl -> Int
getDeclSize (Decl _ _ declVar) =
  case declVar of
    -- Variable case
    (DBaseVar _)
      -> 1
    (ShapeVar _ shape)
      -> case shape of
            -- Array case
            (SArray n)
              -> n
            -- Matrix case
            (SMatrix n m)
              -> n * m

------------------ Program Helper ----------------------

-- Put procedures (recursion)
putProcedures :: [Proc] -> State SymTable ()
putProcedures [] = return ()
putProcedures (p:ps)
  = do
      putProcedure p
      putProcedures ps

-- Put procedure
putProcedure :: Proc -> State SymTable ()
putProcedure (Proc pos ident params _ _)
  = do
      let types = map (\(Param _ i bt _) -> ((i == Val), bt)) params
      insertProcedure ident types pos

-- Check if main procedure exists with no parameter
checkMain :: State SymTable ()
checkMain
  = do
      st <- get
      -- Look up procedure map in symbol table
      let main = Map.lookup "main" (procedures st)
      case main of
        Nothing
          -> do
                error $ "lack of main procedure"
        Just params
          -> do
                -- Check no parameter exists
                if (length params) /= 0
                  then error $ "parameters in main procedure"
                  else return ()

-- Initial symbol table
test :: GoatProg -> ((), SymTable)
test prog
  = runState
      (compileProg prog)
      (SymTable
        { labelCounter = 0
        , slotCounter = 0
        , regCounter = 0
        , code = ""
        , procedures = Map.empty
        , variables = Map.empty
        })

-- Return the compiled oz code
getCode :: ((), SymTable) -> String
getCode (_, st) = code st
