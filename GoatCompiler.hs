-----------------------------------------------------------
-- COMP90045 Programming Language Implementation Project --
--                     Goat Compiler                     --
-- Implemented by Shjie Liu, Wenqing Xue, Minjian Chen   --
-----------------------------------------------------------

-- Goat Compiler for generating assembly language Oz code

module GoatCompiler where

import Control.Monad.State
import Data.Map (Map, (!))
import qualified Data.Map as Map
import GoatAST
import GoatFormat (stmtVarToStr, exprToStr, exprsToStr)
import Text.Parsec.Pos

-- Variable shape is used to distinguish variable type
data VarShape
  = Single
  | Array Int
  | Matrix Int Int
  deriving (Show, Eq)

-- Symbol table is used to record the program state
data SymTable
  = SymTable
    { labelCounter :: Int
    , slotCounter  :: Int
    , regCounter   :: Int
    , code         :: String
    , variables    :: Map String (Bool, BaseType, VarShape, Int)
    , procedures   :: Map String ([(Bool, BaseType)])
    } deriving (Show)


----------- SymTable Helper -----------

-- Return the next available slot number
-- Slot counter increases in symbol table
nextAvailableSlot :: State SymTable Int
nextAvailableSlot
  = do
      st <- get
      let slot = slotCounter st
      put $ st { slotCounter = (slot + 1) }
      return slot

-- Return the next available register number
-- Register counter increases in symbol table
nextAvailableReg :: State SymTable Int
nextAvailableReg
  = do
      st <- get
      let reg = regCounter st
      -- Check register number exceeds 1024
      if reg > 1023 then
        error $ "number of register exceeds 1024"
      else
        do
          put $ st { regCounter = (reg + 1) }
          return reg

-- Return the next available label number
-- Label counter increases in symbol table
nextAvailableLabel :: State SymTable Int
nextAvailableLabel
  = do
      st <- get
      let label = labelCounter st
      put $ st { labelCounter = (label + 1) }
      return label

-- Insert procedure into symbol table
insertProcedure :: String -> [(Bool, BaseType)]
                -> SourcePos -> State SymTable ()
insertProcedure ident types pos
  = do
      st <- get
      -- Check if duplicated procedure exists
      if Map.member ident (procedures st) then
        error $ putPosition pos ++ "Duplicated procedure " ++ ident
      else
        put $ st { procedures = Map.insert ident types (procedures st) }

-- Insert variable into symbol table
insertVariable :: String -> (Bool, BaseType, VarShape, Int)
               -> SourcePos -> State SymTable ()
insertVariable ident (isVal, bt, vs, slot) pos
  = do
      st <- get
      -- Check if duplicated variable exists
      if Map.member ident (variables st) then
        error $ putPosition pos ++ "Duplicate variable " ++ ident
      else
        put $ st { variables
                 = Map.insert ident (isVal, bt, vs, slot) (variables st) }

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

-- Set register counter to n in symbol table
setReg :: Int -> State SymTable ()
setReg n
  = do
      st <- get
      put $ st { regCounter = n }

-- Reset variable map in symbol table
resetVariable :: State SymTable ()
resetVariable
  = do
      st <- get
      put $ st { variables = Map.empty }

-- Reset procedure counters after a procedure in symbol table
resetProcedure :: State SymTable ()
resetProcedure
  = do
      resetReg
      resetSlot
      resetVariable

-- Return the variable with given identifier
getVariable :: String -> VarShape -> SourcePos
            -> State SymTable (Bool, BaseType, VarShape, Int)
getVariable ident vs pos
  = do
      st <- get
      -- Check if variable exists in symbol table
      case Map.lookup ident (variables st) of
        -- Variable of given identifier is undefined
        Nothing
          -> error $ putPosition pos ++ "Undefined variable " ++ ident
        Just (isVal, baseType, varShape, slot)
          -> if sameVarShapeType vs varShape then
                return (isVal, baseType, varShape, slot)
             -- Types are not matched
             else error $  putPosition pos
                        ++ "Type of variable is not matching " ++ ident

-- Check given variable shapes are the same
sameVarShapeType :: VarShape -> VarShape -> Bool
sameVarShapeType Single       Single       = True
sameVarShapeType (Array _)    (Array _)    = True
sameVarShapeType (Matrix _ _) (Matrix _ _) = True
sameVarShapeType _            _            = False

-- Return the procedure with given identifier
getProcedure :: String -> SourcePos -> State SymTable ([(Bool, BaseType)])
getProcedure ident pos
  = do
      st <- get
      -- Check if procedure exists in symbol table
      case Map.lookup ident (procedures st) of
        Nothing
          -> error $ putPosition pos ++ "Undefined procedure " ++ ident
        Just v
          -> return v

-- Return the parameter with given procedure identifier and index
getProcParameter :: String -> Int -> State SymTable (Bool, BaseType)
getProcParameter ident idx
  = do
      st <- get
      return $ (procedures st) ! ident !! idx


---------------------- Common Helper ----------------------

-- Put given string into code string
putCode :: String -> State SymTable ()
putCode str
  = do
      st <- get
      let c = code st
      put $ st { code = c ++ str }

-- Return position string from SourcePos
putPosition :: SourcePos -> String
putPosition pos
  = let
      line = sourceLine pos
      column = sourceColumn pos
    in
      -- Semantics error message with position
      "Semantics error: line " ++ show line
      ++ " column " ++ show column ++ "\n"


----------- Main Compiler -----------

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


----------- Procedure Compiler -----------

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
      unless (null params) (putCode $ "# parameter passing\n")
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

----------- Statement Compiler -----------

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
      -- Check if statement variable is assignable
      if assignable then
        putAssignCode stmtVar regThis pos
      else
        error $ putPosition pos ++ "Assignment types are not matched"

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
                      BoolType  -> "print_bool\n"
                      IntType   -> "print_int\n"
                      FloatType -> "print_real\n"
      -- Call built-in print function
      putCode $ "    call_builtin " ++ func

-- Write string statement
compileStmt (SWrite pos string)
  = do
      reg <- nextAvailableReg
      putStmtComment (SWrite pos string)
      -- Store string into register
      putCode $  "    string_const r" ++ show reg
              ++ ", \"" ++ string ++ "\"" ++ "\n"
      -- Call built-in print string function
      putCode $ "    call_builtin print_string" ++ "\n"

-- Call statement
compileStmt (Call pos ident es)
  = do
      reg <- nextAvailableReg
      proc <- getProcedure ident pos
      -- Check parameter length is matched
      if (length proc) == (length es) then
        do
          putStmtComment (Call pos ident es)
          compileExprs ident reg es
          -- Call ident procedure
          putCode $ "    call proc_" ++ ident ++ "\n"
      else error $ putPosition pos ++ "Call procedure does not matched"

-- If-then statement
compileStmt (If pos expr stmts [])
  = do
      reg <- nextAvailableReg
      afterThen <- nextAvailableLabel
      putStmtComment (If pos expr stmts [])
      exprType <- compileExpr reg expr
      -- If expr must be boolean type
      if exprType == BoolType then
        do
          -- Add code for branch on false
          putCode $ "    branch_on_false r0, label_" ++ show afterThen ++ "\n"
          putCode $ "# then\n"
          compileStmts stmts
          putCode $ "# fi\n"
          -- Add label for branch_on_false
          putCode $ "label_" ++ show afterThen ++ ":\n"
      else
        error $  putPosition pos
              ++ "Expression of If statement can not have type "
              ++ show exprType

-- If-then-else statement
compileStmt (If pos expr thenStmts elseStmts)
  = do
      reg <- nextAvailableReg
      inElse <- nextAvailableLabel
      afterElse <- nextAvailableLabel
      putStmtComment (If pos expr thenStmts elseStmts)
      exprType <- compileExpr reg expr
      -- If expr must be boolean type
      if exprType == BoolType then
        do
          -- Add code for branch on false
          putCode $ "    branch_on_false r0, label_" ++ show inElse ++ "\n"
          putCode $ "# then\n"
          compileStmts thenStmts
          -- Add code for branch uncond
          putCode $ "    branch_uncond label_" ++ show afterElse ++ "\n"
          putCode $ "label_" ++ show inElse ++ ":\n"
          putCode $ "# else\n"
          compileStmts elseStmts
          putCode $ "# fi\n"
          -- Add label for branch_uncond
          putCode $ "label_" ++ show afterElse ++ ":\n"
      else
        error $  putPosition pos
              ++ "Expression of If statement can not have type "
              ++ show exprType

-- While statement
compileStmt (While pos expr stmts)
  = do
      reg <- nextAvailableReg
      inWhile <- nextAvailableLabel
      afterWhile <- nextAvailableLabel
      putStmtComment (While pos expr stmts)
      putCode $ "label_" ++ show inWhile ++ ":\n"
      exprType <- compileExpr reg expr
      -- While expr must be boolean type
      if exprType == BoolType then
        do
          -- Add code for branch on false
          putCode $ "    branch_on_false r0, label_" ++ show afterWhile ++ "\n"
          putCode $ "# do\n"
          compileStmts stmts
          -- Add code for branch uncond
          putCode $ "    branch_uncond label_" ++ show inWhile ++ "\n"
          putCode $ "# od\n"
          -- Add label for branch_on_false
          putCode $ "label_" ++ show afterWhile ++ ":\n"
      else
        error $  putPosition pos
              ++ "Expression of While statement can not have type "
              ++ show exprType

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

----------- Statement Helper -----------

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
compileStmtComment (Read _ stmtVar)
  = "read " ++ stmtVarToStr stmtVar ++ ";"
compileStmtComment (Write _ expr)
  = "write " ++ exprToStr False expr ++ ";"
compileStmtComment (SWrite _ string)
  = "write \"" ++ string ++ "\";"
compileStmtComment (Call _ ident exprs)
  = "call " ++ ident ++ "(" ++ exprsToStr exprs ++ ");"
compileStmtComment (If _ expr _ _)
  = "if " ++ exprToStr False expr
compileStmtComment (While _ expr _)
  = "while " ++ exprToStr False expr

-- Return the base type of given statement variable
getStmtVarBaseType :: StmtVar -> SourcePos -> State SymTable BaseType
-- Single variable case
getStmtVarBaseType (SBaseVar ident) pos
  = do
      (_, btype, _, _) <- getVariable ident (Single) pos
      return btype
-- Array or Matrix variable case
getStmtVarBaseType (IndexVar ident index) pos
  = do
      (_, btype, _, _) <- getVariable ident (convertIndex2VarShape index) pos
      return btype

-- Convert Index type into VarShape type
convertIndex2VarShape :: Index -> VarShape
convertIndex2VarShape (IArray _)    = (Array 0)
convertIndex2VarShape (IMatrix _ _) = (Matrix 0 0)

-- Check if two types can be assigned
-- Note: float can be assigned by int
assignableType :: BaseType -> BaseType -> Int -> State SymTable Bool
assignableType FloatType IntType reg
  = do
      -- Convert int to float type
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
        -- Call by reference
        putAssignCodeRef slot reg
      else
        -- Call by value
        -- Store assigned value into original slot
        putCode $ "    store " ++ show slot ++ ", r" ++ show reg ++ "\n"
-- Array case
putAssignCode (IndexVar ident (IArray expr)) reg pos
  = do
      (_, _, _, slot) <- getVariable ident (Array 0) pos
      offsetReg <- nextAvailableReg
      exprType <- compileExpr offsetReg expr
      -- Check if index is int type
      if exprType == IntType then
        putAssignCodeOffset offsetReg slot reg
      else
        error $ putPosition pos ++ "Array index is not integer"
-- Matrix case
putAssignCode (IndexVar ident (IMatrix expr1 expr2)) reg pos
  = do
      (_, _, (Matrix row col), slot) <- getVariable ident (Matrix 0 0) pos
      offsetReg <- nextAvailableReg
      colReg <- nextAvailableReg
      expr1Type <- compileExpr offsetReg expr1
      expr2Type <- compileExpr colReg expr2
      -- Check if indexes are int type
      if expr1Type == IntType && expr2Type == IntType then
        do
          putSetOffsetReg offsetReg colReg col
          putAssignCodeOffset offsetReg slot reg
      else
        error $ putPosition pos ++ "Matrix index is not integer"

-- Put calculation of offset register for Matrix
putSetOffsetReg :: Int -> Int -> Int -> State SymTable ()
putSetOffsetReg offsetReg colReg col
  = do
      sizeReg <- nextAvailableReg
      -- Store column value into available register
      putCode $  "    int_const r" ++ show sizeReg ++ ", " ++ show col ++ "\n"
      -- Multiply with offset value into offset register
      putCode $  "    mul_int r" ++ show offsetReg ++ ", r"
              ++ show offsetReg ++ ", r" ++ show sizeReg ++ "\n"
      -- Add column value into offset register
      putCode $  "    add_int r" ++ show offsetReg ++ ", r"
              ++ show offsetReg ++ ", r" ++ show colReg ++ "\n"

-- Put calculation of stack array's index
putAssignCodeOffset :: Int -> Int -> Int -> State SymTable ()
putAssignCodeOffset offsetReg startSlot reg
  = do
      addrReg <- nextAvailableReg
      -- Load address of start slot
      putCode $  "    load_address r" ++ show addrReg ++ ", "
              ++ show startSlot ++ "\n"
      -- Subtract address with offset
      putCode $  "    sub_offset r" ++ show addrReg ++ ", r"
              ++ show addrReg ++ ", r" ++ show offsetReg ++ "\n"
      -- Store indirect to address
      putCode $  "    store_indirect r" ++ show addrReg
              ++ ", r" ++ show reg ++ "\n"

-- Put assignment code for Ref Indicator
putAssignCodeRef :: Int -> Int -> State SymTable ()
putAssignCodeRef addrSlot reg
  = do
      addrReg <- nextAvailableReg
      -- Load address to register
      putCode $  "    load r" ++ show addrReg ++ ", "
              ++ show addrSlot ++ "\n"
      -- Store indirect to address
      putCode $  "    store_indirect r" ++ show addrReg
              ++ ", r" ++ show reg ++ "\n"

-- Put read instruction based on base type of variable
putReadCodeType :: BaseType -> State SymTable ()
putReadCodeType baseType
  = do
      let func = case baseType of
                      BoolType  -> "read_bool\n"
                      IntType   -> "read_int\n"
                      FloatType -> "read_real\n"
      -- Call built-in read function
      putCode $ "    call_builtin " ++ func

-- Store the statement variable address to a specifity register
storeAddressToRegN :: StmtVar -> Int -> SourcePos -> State SymTable ()
-- Case of the variable is not shaped
storeAddressToRegN (SBaseVar ident) destReg pos
  = do
      (isVal, _, _, slot) <- getVariable ident (Single) pos
      if not isVal then
        -- Load slot number to register
        putCode $ "    load r" ++ show destReg ++ ", " ++ show slot ++ "\n"
      else
        -- Load address to register
        putCode $  "    load_address r" ++ show destReg
                ++ ", " ++ show slot ++ "\n"
-- Case of the variable is array
storeAddressToRegN (IndexVar ident (IArray expr)) destReg pos
  = do
      (_, _, _, slot) <- getVariable ident (Array 0) pos
      offsetReg <- nextAvailableReg
      exprType <- compileExpr offsetReg expr
      if exprType == IntType then
        putStoreAddressCodeOffset offsetReg slot destReg
      else
        error $ putPosition pos ++ "Array index is not integer"
-- Case of the variable is matrix
storeAddressToRegN (IndexVar ident (IMatrix expr1 expr2)) destReg pos
  = do
      (_, _, (Matrix row col), slot) <- getVariable ident (Matrix 0 0) pos
      offsetReg <- nextAvailableReg
      colReg <- nextAvailableReg
      expr1Type <- compileExpr offsetReg expr1
      expr2Type <- compileExpr colReg expr2
      if expr1Type == IntType && expr2Type == IntType then
        do
          putSetOffsetReg offsetReg colReg col
          putStoreAddressCodeOffset offsetReg slot destReg
      else
        error $ putPosition pos ++ "Matrix index is not integer"

-- Add the code of storing slot address into given register
putStoreAddressCodeOffset :: Int -> Int -> Int -> State SymTable ()
putStoreAddressCodeOffset offsetReg startSlot destReg
  = do
      -- Load address of start slot
      putCode $  "    load_address r" ++ show destReg
              ++ ", " ++ show startSlot ++ "\n"
      -- Calculate the offset
      putCode $  "    sub_offset r" ++ show destReg ++ ", r"
              ++ show destReg ++ ", r" ++ show offsetReg ++ "\n"

----------- Expression Compiler -----------

-- Compile boolean constant expression
-- Use int_const to store boolean value
-- If true then store 1, otherwise 0
compileExpr :: Int -> Expr -> State SymTable BaseType
compileExpr reg (BoolConst _ bool)
  = do
      let boolInt = if bool then 1 else 0
      -- Initial bool constant
      putCode $ "    int_const r" ++ show reg ++ ", " ++ show boolInt ++ "\n"
      return BoolType

-- Compile integer constant expression
compileExpr reg (IntConst _ i)
  = do
      -- Initial int constant
      putCode $ "    int_const r" ++ show reg ++ ", " ++ show i ++ "\n"
      return IntType

-- Compile float constant expression
compileExpr reg (FloatConst _ f)
  = do
      -- Initial flost constant
      putCode $ "    real_const r" ++ show reg ++ ", " ++ show f ++ "\n"
      return FloatType

-- Compile binary add operation expression
compileExpr reg (Add pos expr1 expr2)
  -- add_int / add_real
  = compileArithmetricExpr "add" reg expr1 expr2 pos

-- Compile binary minus operation expression
compileExpr reg (Minus pos expr1 expr2)
  -- sub_int / sub_real
  = compileArithmetricExpr "sub" reg expr1 expr2 pos

-- Compile binary multiply operation expression
compileExpr reg (Mul pos expr1 expr2)
  -- mul_int / mul_real
  = compileArithmetricExpr "mul" reg expr1 expr2 pos

-- Compile binary division operation expression
compileExpr reg (Div pos expr1 expr2)
  -- div_int / div_real
  = compileArithmetricExpr "div" reg expr1 expr2 pos

-- Compile binary or operation expression
compileExpr reg (Or pos expr1 expr2)
  -- or
  = compileLogicalExpr "or" "true" reg expr1 expr2 pos

-- Compile binary and operation expression
compileExpr reg (And pos expr1 expr2)
  -- and
  = compileLogicalExpr "and" "false" reg expr1 expr2 pos

-- Compile binary equal operation expression
compileExpr reg (Equal pos expr1 expr2)
  -- cmp_eq_int / cmp_eq_real
  = compileEqualityExpr "eq" reg expr1 expr2 pos

-- Compile binary not equal operation expression
compileExpr reg (NotEqual pos expr1 expr2)
  -- cmp_ne_int / cmp_ne_real
  = compileEqualityExpr "ne" reg expr1 expr2 pos

-- Compile binary greater operation expression
compileExpr reg (Greater pos expr1 expr2)
  -- cmp_gt_int / cmp_gt_real
  = compileCompareExpr "gt" reg expr1 expr2 pos

-- Compile binary greater equal operation expression
compileExpr reg (GreaterEqual pos expr1 expr2)
  -- cmp_ge_int / cmp_ge_real
  = compileCompareExpr "ge" reg expr1 expr2 pos

-- Compile binary less operation expression
compileExpr reg (Less pos expr1 expr2)
  -- cmp_lt_int / cmp_lt_real
  = compileCompareExpr "lt" reg expr1 expr2 pos

-- Compile binary less equal operation expression
compileExpr reg (LessEqual pos expr1 expr2)
  -- cmp_le_int / cmp_le_real
  = compileCompareExpr "le" reg expr1 expr2 pos

-- Compile unary negation operation expression
-- Negation is only for boolean variable type
compileExpr reg (Neg pos expr)
  = do
      baseType <- compileExpr reg expr
      if baseType == BoolType then
        do
          -- Negate value if base type is boolean
          putCode $ "    not r" ++ show reg ++ ", r" ++ show reg ++ "\n"
          return BoolType
      else
        error $ putPosition pos ++ "Can not negate type " ++ show baseType

-- Compile unary minus operation expression
-- Unary mainus is only for int and float variable type
compileExpr reg (UMinus pos expr)
  = do
      baseType <- compileExpr reg expr
      case baseType of
        IntType
          -> do
                -- Inverse value if base type is int
                putCode $  "    neg_int r" ++ show reg
                        ++ ", r" ++ show reg ++ "\n"
                return baseType
        FloatType
          -> do
                -- Inverse value if base type is float
                putCode $  "    neg_real r" ++ show reg
                        ++ ", r" ++ show reg ++ "\n"
                return baseType
        _
          -> error $ putPosition pos
                  ++ "Can not uminus type " ++ show baseType

-- Compile Id expression where Id is a base statement variable
compileExpr reg (Id pos (SBaseVar ident))
  = do
      (isVal, baseType, varShape, slot) <- getVariable ident (Single) pos
      if varShape == Single then
        -- Call by value
        if isVal then
          do
            -- Load value from slot into register
            putCode $ "    load r" ++ show reg ++ ", " ++ show slot ++ "\n"
            return baseType
        -- Call by reference
        else
          do
            -- Load value from slot into register
            putCode $ "    load r" ++ show reg ++ ", " ++ show slot ++ "\n"
            -- Load address value from register into register
            putCode $  "    load_indirect r" ++ show reg ++ ", r"
                    ++ show reg ++ "\n"
            return baseType
      else
        error $  putPosition pos ++ "Expected type " ++ show varShape
              ++ ", while type Single received"

-- Compile Id expression where Id is a array statement variable
compileExpr reg (Id pos (IndexVar ident (IArray expr)))
  = do
      (_, baseType, varShape, slot) <- getVariable ident (Array 0) pos
      reg1 <- nextAvailableReg
      exprType <- compileExpr reg1 expr
      case varShape of
        (Array n)
          -> do
                -- Check array index type
                if exprType == IntType then
                  do
                    -- Load address from slot into register
                    putCode $  "    load_address r" ++ show reg ++ ", "
                            ++ show slot ++ "\n"
                    -- Calculate address using offset
                    putCode $  "    sub_offset r" ++ show reg ++ ", r"
                            ++ show reg ++ ", r" ++ show reg1 ++ "\n"
                    -- Load value indirectly to address
                    putCode $  "    load_indirect r" ++ show reg ++ ", r"
                            ++ show reg ++ "\n"
                    return baseType
                else
                  error $  putPosition pos
                        ++ "Array Index must be IntType, while type "
                        ++ show exprType ++ " received"
        Single
          -> error $ putPosition pos
          ++ "Expect Single expression, while Array expression is given"
        (Matrix _ _)
          -> error $ putPosition pos
          ++ "Expect Matrix expression, while Array expression is given"

-- Compile Id expression where Id is a matrix statement variable
compileExpr reg (Id pos (IndexVar ident (IMatrix expr1 expr2)))
  = do
      (_, baseType, varShape, slot) <- getVariable ident (Matrix 0 0) pos
      reg1 <- nextAvailableReg
      reg2 <- nextAvailableReg
      type1 <- compileExpr reg1 expr1
      type2 <- compileExpr reg2 expr2
      case varShape of
        (Matrix a b)
          -> do
                -- Check matrix indexes type
                if type1 == IntType && type2 == IntType then
                  do
                    reg3 <- nextAvailableReg
                    -- calculate the offset using the index of the matrix
                    putCode $  "    int_const r" ++ show reg3 ++ ", "
                            ++ show b ++ "\n"
                    putCode $  "    mul_int r" ++ show reg3 ++ ", r"
                            ++ show reg3 ++ ", r" ++ show reg1 ++ "\n"
                    putCode $  "    add_int r" ++ show reg3 ++ ", r"
                            ++ show reg3 ++ ", r" ++ show reg2 ++ "\n"
                    putCode $  "    load_address r" ++ show reg ++ ", "
                            ++ show slot ++ "\n"
                    putCode $  "    sub_offset r" ++ show reg ++ ", r"
                            ++ show reg ++ ", r" ++ show reg3 ++ "\n"
                    -- load the value stored in the slot with the address
                    putCode $  "    load_indirect r" ++ show reg ++ ", r"
                            ++ show reg ++ "\n"
                    return baseType
                else
                  error $  putPosition pos
                        ++ "Matrix Index must be IntType, while type "
                        ++ show type1 ++ " and type "
                        ++ show type2 ++ " received"
        (Single)
          -> error $ putPosition pos
          ++ "Expect Single expression, while Matrix expression is given"
        (Array _)
          -> error $ putPosition pos
          ++ "Expect Array expression, while Matrix expression is given"

-- Compile a list of expressions and store given
-- results into registers starting from r0
compileExprs :: String -> Int -> [Expr] -> State SymTable ()
compileExprs _ _ [] = return ()
compileExprs ident n (e:es)
  = do
      let pos = getExprPos e
      (isVal, baseType) <- getProcParameter ident n
      if isVal then
        -- Call by value
        do
          exprType <- compileExpr n e
          setReg (n + 1)
          if exprType == baseType then
            do
              n1 <- nextAvailableReg
              compileExprs ident n1 es
          else
            if exprType == IntType && baseType == FloatType then
              do
                -- Convert int to float type
                putCode $  "    int_to_real r" ++ show n
                        ++ ", r" ++ show n ++ "\n"
                n1 <- nextAvailableReg
                compileExprs ident n1 es
            else error $ putPosition pos
                      ++ "Procedure parameter does not match"
      else
        -- Call by reference
        case e of
          (Id _ stmtVar)
            -> do
                  stmtVarType <- getStmtVarBaseType stmtVar pos
                  if stmtVarType == baseType then
                    do
                      storeAddressToRegN stmtVar n pos
                      setReg (n + 1)
                      n1 <- nextAvailableReg
                      compileExprs ident n1 es
                  else
                    error $  putPosition pos
                          ++ "Procedure parameter does not match"
          _ -> error $ putPosition pos
                    ++ "Ref procedure parameter does not allow Non-lvalue"

----------- Expression Helper -----------
-- Compile arithmetric expression, including
-- "add", "minus", "multip1y" and "divide"
compileArithmetricExpr :: String -> Int -> Expr -> Expr
                       -> SourcePos -> State SymTable BaseType
compileArithmetricExpr s reg expr1 expr2 pos
  = do
      reg1 <- nextAvailableReg
      type1 <- compileExpr reg expr1
      type2 <- compileExpr reg1 expr2
      if type1 == type2 then
        if type1 == IntType then
          do
            -- oz arithmetic operation for int
            putCode $  "    " ++ s ++ "_int r" ++ show reg ++ ", r"
                    ++ show reg ++ ", r" ++ show reg1 ++ "\n"
            return IntType
        else
          if type1 == FloatType then
            do
              -- oz arithmetic operation for float
              putCode $  "    " ++ s ++ "_real r" ++ show reg ++ ", r"
                      ++ show reg ++ ", r" ++ show reg1 ++ "\n"
              return FloatType
          else
            error $  putPosition pos ++ "Can not " ++ s ++ " type "
                  ++ show type1 ++ " with type " ++ show type2
      else
        if type1 == IntType && type2 == FloatType then
          do
            -- convert int type to float type
            putCode $  "    int_to_real r" ++ show reg ++ ", r"
                    ++ show reg ++ "\n"
            -- oz arithmetic operation for float
            putCode $  "    " ++ s ++ "_real r" ++ show reg ++ ", r"
                    ++ show reg ++ ", r" ++ show reg1 ++ "\n"
            return FloatType
        else
          if type1 == FloatType && type2 == IntType then
            do
              -- convert int type to float type
              putCode $  "    int_to_real r" ++ show reg1 ++ ", r"
                      ++ show reg1 ++ "\n"
              -- oz arithmetic operation for float
              putCode $  "    " ++ s ++ "_real r" ++ show reg ++ ", r"
                      ++ show reg ++ ", r" ++ show reg1 ++ "\n"
              return FloatType
          else
            error $  putPosition pos ++ "Can not " ++ s ++ " type "
                  ++ show type1 ++ " with type " ++ show type2

-- Compile equality expression, including "=" and "!="
compileEqualityExpr :: String -> Int -> Expr -> Expr
                    -> SourcePos -> State SymTable BaseType
compileEqualityExpr s reg expr1 expr2 pos
  = do
      reg1 <- nextAvailableReg
      type1 <- compileExpr reg expr1
      type2 <- compileExpr reg1 expr2
      if type1 == type2 then
        if type1 == FloatType then
          do
            -- Compare float value
            putCode $  "    cmp_" ++ s ++ "_real" ++ " r" ++ show reg
                    ++ ", r" ++ show reg ++ ", r" ++ show reg1 ++ "\n"
            return BoolType
        else
          do
            -- Compare int value
            putCode $  "    cmp_" ++ s ++ "_int" ++ " r" ++ show reg ++ ", r"
                    ++ show reg ++ ", r" ++ show reg1 ++ "\n"
            return BoolType
      else
        error $  putPosition pos ++ "Can not compare " ++ s
              ++ " with type " ++ show type1 ++ " and type " ++ show type2

-- Compile logical expression, including "and" and "or"
compileLogicalExpr :: String -> String -> Int -> Expr -> Expr
                   -> SourcePos -> State SymTable BaseType
compileLogicalExpr s boolstr reg expr1 expr2 pos
  = do
      reg1 <- nextAvailableReg
      after <- nextAvailableLabel
      type1 <- compileExpr reg expr1
      -- jump to the label beacuse of short circuit
      putCode $  "    branch_on_" ++ boolstr ++ " r"
              ++ show reg ++ ", label_" ++ show after ++ "\n"
      type2 <- compileExpr reg1 expr2
      -- compile logical expression
      putCode $  "    " ++ s ++ " r" ++ show reg ++ ", r"
              ++ show reg ++ ", r" ++ show reg1 ++ "\n"
      -- put the label for short circuit
      putCode $  "label_" ++ show after ++ ":\n"
      if type1 == BoolType && type2 == BoolType then
        return BoolType
      else
        error $  putPosition pos ++ s
              ++ " operation can not be used between "
              ++ show type1 ++ " and " ++ show type2

-- Compile compare expression, including ">=", "<=", ">", "<"
compileCompareExpr :: String -> Int -> Expr -> Expr
                   -> SourcePos -> State SymTable BaseType
compileCompareExpr s reg expr1 expr2 pos
  = do
      reg1 <- nextAvailableReg
      type1 <- compileExpr reg expr1
      type2 <- compileExpr reg1 expr2
      if type1 == type2 then
        if type1 == FloatType then
          do
            -- Compare float value
            putCode $  "    cmp_" ++ s ++ "_real r" ++ show reg ++ ", r"
                    ++ show reg ++ ", r" ++ show reg1 ++ "\n"
            return BoolType
        else
          do
            -- Compare int value
            putCode $  "    cmp_" ++ s ++ "_int r" ++ show reg ++ ", r"
                    ++ show reg ++ ", r" ++ show reg1 ++ "\n"
            return BoolType
      else
        if type1 == IntType && type2 == FloatType then
          do
              -- Convert int to float type, then compare float value
              putCode $ "    int_to_real r" ++ show reg ++ ", r"
                      ++ show reg ++ "\n"
              putCode $  "    cmp_" ++ s ++ "_real r" ++ show reg ++ ", r"
                      ++ show reg ++ ", r" ++ show reg1 ++ "\n"
              return BoolType
        else
          if type1 == FloatType && type2 == IntType then
            do
              -- Convert int to float type, then compare float value
              putCode $  "    int_to_real r" ++ show reg1 ++ ", r"
                      ++ show reg1 ++ "\n"
              putCode $  "    cmp_" ++ s ++ "_real r" ++ show reg ++ ", r"
                      ++ show reg ++ ", r" ++ show reg1 ++ "\n"
              return BoolType
          else
              error $  putPosition pos ++ "Can not compare" ++ s
                    ++ " with type " ++ show type1
                    ++ " and type " ++ show type2

-- Get position from given expression
getExprPos :: Expr -> SourcePos
getExprPos (Id pos _)             = pos
getExprPos (BoolConst pos _)      = pos
getExprPos (IntConst pos _)       = pos
getExprPos (FloatConst pos _)     = pos
getExprPos (Add pos _ _)          = pos
getExprPos (Minus pos _ _)        = pos
getExprPos (Mul pos _ _)          = pos
getExprPos (Div pos _ _)          = pos
getExprPos (Or pos _ _)           = pos
getExprPos (And pos _ _)          = pos
getExprPos (Equal pos _ _)        = pos
getExprPos (NotEqual pos _ _)     = pos
getExprPos (Less pos _ _)         = pos
getExprPos (LessEqual pos _ _)    = pos
getExprPos (Greater pos _ _)      = pos
getExprPos (GreaterEqual pos _ _) = pos
getExprPos (Neg pos _)            = pos
getExprPos (UMinus pos _)         = pos

----------- Declartion Helper -----------
-- Compile the declarations of a procedure
putDeclarations :: [Decl] -> State SymTable ()
putDeclarations [] = return ()
putDeclarations ds
  = do
      ri <- nextAvailableReg
      -- Initial int value 0 for declaration
      putCode $ "    int_const r" ++ show ri ++ ", 0\n"
      rf <- nextAvailableReg
      -- Initial float value 0.0 for declaration
      putCode $ "    real_const r" ++ show rf ++ ", 0.0\n"
      putDeclarations' ri rf ds

-- Compile the declarations of a precedure using the given two register number
putDeclarations' :: Int -> Int -> [Decl] -> State SymTable ()
putDeclarations' _ _ [] = return ()
putDeclarations' ri rf (d:ds)
  = do
      let r = whichDeclReg ri rf d
      putDeclaration' r d
      putDeclarations' ri rf ds

-- Compile a declaration
putDeclaration' :: Int -> Decl -> State SymTable ()
putDeclaration' r (Decl pos baseType declVar)
  = do
      case declVar of
        (DBaseVar ident)
          -> do
                putDeclComment (DBaseVar ident) baseType
                slot <- nextAvailableSlot
                insertVariable ident (True, baseType, Single, slot) pos
                -- Store a register value into a slot
                putCode $ "    store " ++ show slot ++ ", r" ++ show r ++ "\n"
        (ShapeVar ident shape)
          -> case shape of
              (SArray num)
                -> do
                      putDeclComment (ShapeVar ident (SArray num)) baseType
                      slot <- nextAvailableSlot
                      insertVariable ident (True, baseType,
                                            (Array num), slot) pos
                      -- Store a register value into a slot
                      putCode $  "    store " ++ show slot
                              ++ ", r" ++ show r ++ "\n"
                      putFilledSkipSlot r (num - 1)
              (SMatrix row col)
                -> do
                      putDeclComment (ShapeVar ident (shape)) baseType
                      slot <- nextAvailableSlot
                      insertVariable ident (True, baseType,
                                            (Matrix row col), slot) pos
                      -- Store a register value into a slot
                      putCode $  "    store " ++ show slot
                              ++ ", r" ++ show r ++ "\n"
                      putFilledSkipSlot r (row * col - 1)

-- skip the slot number for
putFilledSkipSlot :: Int -> Int -> State SymTable ()
putFilledSkipSlot _ 0 = return ()
putFilledSkipSlot r num
  = do
      slot <- nextAvailableSlot
      -- store a register value into a slot
      putCode $ "    store " ++ show slot ++ ", r" ++ show r ++ "\n"
      putFilledSkipSlot r (num - 1)

-- Put declaration comment
putDeclComment :: DeclVar -> BaseType -> State SymTable ()
putDeclComment declVar baseType
  = do
      -- Base type string
      let tStr = case baseType of
                      IntType -> "int"
                      BoolType -> "bool"
                      FloatType -> "float"
      -- Variable string
      let iStr = case declVar of
                      (DBaseVar ident)
                        -> ident
                      -- Array [n]
                      (ShapeVar ident (SArray num))
                        -> ident ++ "[" ++ show num ++ "]"
                      -- Matrix [m,n]
                      (ShapeVar ident (SMatrix row col))
                        -> ident ++ "[" ++ show row ++ "," ++ show col ++ "]"
      putCode $ "# initialise " ++ tStr ++ " val " ++ iStr ++ "\n"
      return ()

--determine which register to use according to the declaration base type
whichDeclReg :: Int -> Int -> Decl -> Int
whichDeclReg ri rf (Decl _ baseType _)
  = if baseType == FloatType then rf else ri

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
      -- Store from register into slot
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
      putCode "# prologue\n"
      let ps = length params
      let ds = getDeclsSize decls
      -- Push the total size needed from declarations
      putCode $ "    push_stack_frame " ++ show (ps + ds) ++ "\n"
      return (ps + ds)

-- Put procedure epilogue
putProcedureEpilogue :: Int -> State SymTable ()
putProcedureEpilogue n
  = do
      putCode "# epilogue\n"
      -- Pop the total size
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
    -- Single variable
    (DBaseVar _) -> 1
    (ShapeVar _ shape) -> case shape of
                            -- Array variable
                            (SArray n) -> n
                            -- Matrix variable
                            (SMatrix n m) -> n * m

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
initial :: GoatProg -> ((), SymTable)
initial prog
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
