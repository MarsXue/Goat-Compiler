module GoatCompiler (test, getCode) where

import Control.Monad.State
import Data.Map (
    Map,
    (!),
    )
import qualified Data.Map as Map
import GoatAST
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


nextAvailableSlot :: State SymTable Int
nextAvailableSlot
    = do
        st <- get
        let slot = slotCounter st
        put $ st {slotCounter = (slot + 1)}
        return slot

nextAvailableReg :: State SymTable Int
nextAvailableReg
    = do
        st <- get
        let reg = regCounter st
        if reg > 1023
            then error $ "number of register exceeds 1023"
            else
                do
                    put $ st {regCounter = (reg + 1)}
                    return reg

nextAvailableLabel :: State SymTable Int
nextAvailableLabel
    = do
        st <- get
        let label = labelCounter st
        put $ st {labelCounter = (label + 1)}
        return label

insertProcedure :: String -> [(Bool, BaseType)] -> SourcePos -> State SymTable ()
insertProcedure ident types pos
    = do
        st <- get
        if Map.member ident (procedures st)
            then error $ putPosition pos ++ "duplicated procedure " ++ ident
            else put $ st { procedures = Map.insert ident types (procedures st) }


insertVariable :: String -> (Bool, BaseType, VarShape, Int) -> SourcePos -> State SymTable ()
insertVariable ident (isVal, bt, vs, slot) pos
    = do
        st <- get
        if Map.member ident (variables st)
            then error $ putPosition pos ++ "duplicate variable name " ++ ident
            else put $ st {variables = Map.insert ident (isVal, bt, vs, slot) (variables st)}

resetSlot :: State SymTable ()
resetSlot
    = do
        st <- get
        put $ st {slotCounter = 0}

resetReg :: State SymTable ()
resetReg
    = do
        st <- get
        put $ st {regCounter = 0}

resetVariables :: State SymTable ()
resetVariables
    = do
        st <- get
        put $ st {variables = Map.empty}

resetProcedure :: State SymTable ()
resetProcedure
    = do
        resetReg
        resetSlot
        resetVariables

getVariable :: String -> VarShape -> SourcePos -> State SymTable (Bool, BaseType, VarShape, Int)
getVariable ident vs pos
    = do
        st <- get
        case Map.lookup ident (variables st) of
            Nothing -> error $ putPosition pos ++ "undefined variable " ++ ident
            Just (isVal, baseType, varShape, slot) 
                -> if sameVarShapeType vs varShape 
                    then return (isVal, baseType, varShape, slot)
                    else error $ putPosition pos ++ "type of variable is not matching " ++ ident

sameVarShapeType :: VarShape -> VarShape -> Bool
sameVarShapeType Single Single = True
sameVarShapeType (Array _) (Array _) = True
sameVarShapeType (Matrix _ _) (Matrix _ _) = True
sameVarShapeType _ _ = False

getProcdure :: String -> SourcePos -> State SymTable ([(Bool, BaseType)])
getProcdure ident pos
    = do
        st <- get
        case Map.lookup ident (procedures st) of
            Nothing -> error $ putPosition pos ++ "undefined procedure " ++ ident
            Just v -> return v

getProcParameter :: String -> Int -> State SymTable (Bool, BaseType)
getProcParameter ident idx
    = do
        st <- get
        return $ (procedures st) ! ident !! idx

-----------Main Compiler-----------

compileProg :: GoatProg -> State SymTable ()
compileProg (Prog ps)
    = do
        putCode $ "    call proc_main\n    halt\n"

        putProcedures ps
        checkMain

        compileProcedures ps

-----------Procedure Compiler-----------


compileProcedures :: [Proc] -> State SymTable ()
compileProcedures [] = return ()
compileProcedures (p:ps)
    = do
        compileProcedure p
        compileProcedures ps



compileProcedure :: Proc -> State SymTable ()
compileProcedure (Proc _ ident params decls stmts)
    = do
        -- put label
        putProcedureLabel ident
        -- put prologue
        stackSize <- putProcedurePrologue params decls
        -- parameter passing
        putComments "Passing parameters"
        putParameters params
        resetReg


        -- initialise decls
        putComments "Initialise Declaration"
        putDeclarations decls
        resetReg


        -- put statements
        putComments "Compile Statements"
        putStatements stmts
        resetReg

        -- put epilogue
        putProcedureEpilogue stackSize
        resetProcedure
        return ()



----------------------Helper----------------------

putCode :: String -> State SymTable ()
putCode s
    = do
        st <- get
        let c = code st
        put $ st { code = c ++ s }

putComments :: String -> State SymTable ()
putComments cmt
    = do
        putCode $ "  # " ++ cmt ++ "\n"


putPosition :: SourcePos -> String
putPosition pos
    = 
        let 
            line = sourceLine pos
            column = sourceColumn pos
        in
            "Line " ++ show line ++ " Column " ++ show column ++ "\n"


----------- Statement Helper -----------




putStmtLabel :: Int -> State SymTable ()
putStmtLabel a
    = do
        putCode ("label_" ++ show a ++ ":\n")

putStatements :: [Stmt] -> State SymTable ()
putStatements [] = return ()
putStatements (s:ss)
    = do
        putStatement s
        putStatements ss

putStatement :: Stmt -> State SymTable ()
putStatement s
    = do
        putComments $ show s
        compileStmt s
        resetReg

compileStmts :: [Stmt] -> State SymTable ()
compileStmts [] = return ()
compileStmts (s:stmts)
    = do
        compileStmt s
        compileStmts stmts
---------------------------------------------------------------------------------------------------------------------

getStmtVarBaseType :: StmtVar -> SourcePos -> State SymTable BaseType
getStmtVarBaseType (SBaseVar ident) pos 
    = do
        (isVal, baseType, varShape, slot) <- getVariable ident (Single) pos 
        return baseType
getStmtVarBaseType (IndexVar ident index) pos
    = do
        (isVal, baseType, varShape, slot) <- getVariable ident (convertIndex2VarShape index) pos
        return baseType

convertIndex2VarShape :: Index -> VarShape
convertIndex2VarShape (IArray _) = (Array 0)
convertIndex2VarShape (IMatrix _ _) = (Matrix 0 0) 

assignableType :: BaseType -> BaseType -> Bool
assignableType FloatType IntType = True
assignableType st ex = st == ex

putAssignCode :: StmtVar -> Int -> SourcePos -> State SymTable ()
putAssignCode (SBaseVar ident) reg pos
    = do
        (isVal, baseType, varShape, slot) <- getVariable ident (Single) pos
        if not isVal 
            then putAssignCodeRef slot reg
            else putCode $ "    store " ++ show slot ++ ", r" ++ show reg ++ "\n"

putAssignCode (IndexVar ident (IArray expr)) reg pos
    = do 
        (isVal, baseType, varShape, slot) <- getVariable ident (Array 0) pos
        offsetReg <- nextAvailableReg
        exprType <- compileExpr offsetReg expr
        if exprType == IntType 
            then putAssignCodeOffset offsetReg slot reg
            else error $ putPosition pos ++ "array index is not Int"

putAssignCode (IndexVar ident (IMatrix expr1 expr2)) reg pos
    = do
        (isVal, baseType, (Matrix row col), slot) <- getVariable ident (Matrix 0 0) pos
        offsetReg <- nextAvailableReg
        colReg <- nextAvailableReg
        expr1Type <- compileExpr offsetReg expr1
        expr2Type <- compileExpr colReg expr2
        if expr1Type == IntType && expr2Type == IntType
            then 
                do
                    putSetOffsetReg offsetReg colReg col
                    putAssignCodeOffset offsetReg slot reg
            else error $ putPosition pos ++ "array index is not Int"

putSetOffsetReg :: Int -> Int -> Int -> State SymTable ()
putSetOffsetReg offsetReg colReg col
    = do
        sizeReg <- nextAvailableReg
        putCode $ "    int_const r" ++ show sizeReg ++ ", " ++ show col ++ "\n"
        putCode $ "    mul_int r" ++ show offsetReg ++ ", r" ++ show offsetReg ++ ", r" ++ show sizeReg ++ "\n"
        putCode $ "    add_int r" ++ show offsetReg ++ ", r" ++ show offsetReg ++ ", r" ++ show colReg ++ "\n"

putAssignCodeOffset :: Int -> Int -> Int -> State SymTable ()
putAssignCodeOffset offsetReg startSlot reg
    = do
        addrReg <- nextAvailableReg
        putCode $ "    load_address r" ++ show addrReg ++ ", " ++ show startSlot ++ "\n"
        putCode $ "    sub_offset r" ++ show addrReg ++ ", r" ++ show addrReg ++ ", r" ++ show offsetReg ++ "\n"
        putCode $ "    store_indirect r" ++ show addrReg ++ ", r" ++ show reg ++ "\n"

putAssignCodeRef :: Int -> Int -> State SymTable ()
putAssignCodeRef addrSlot reg
    = do
        addrReg <- nextAvailableReg
        putCode $ "    load r" ++ show addrReg ++ ", " ++ show addrSlot ++ "\n"
        putCode $ "    store_indirect r" ++ show addrReg ++ ", r" ++ show reg ++ "\n"


putReadCodeType :: BaseType -> State SymTable ()
putReadCodeType baseType
    = do
        putCode ("    call_builtin ")
        case baseType of
            BoolType
                -> do
                    putCode ("read_bool\n")
            IntType
                -> do
                    putCode ("read_int\n")
            FloatType
                -> do
                    putCode ("read_real\n")

---------------------------------------------------------------------------------------------------------------------
compileStmt :: Stmt -> State SymTable ()
-- Assign statement
compileStmt (Assign pos stmtVar expr)
    = do
        regThis <- nextAvailableReg
        exprType <- compileExpr regThis expr
        stmtType <- getStmtVarBaseType stmtVar pos
        if assignableType stmtType exprType 
            then putAssignCode stmtVar regThis pos
            else error $ putPosition pos ++ "assginment type dose not match" 

-- Read statement
compileStmt (Read pos stmtVar)
    = do
        reg <- nextAvailableReg
        baseType <- getStmtVarBaseType stmtVar pos
        putReadCodeType baseType
        putAssignCode stmtVar reg pos

-- Write statement
compileStmt (Write pos expr)
    = do
        baseType <- compileExpr 0 expr
        let func = case baseType of
                        BoolType -> "print_bool"
                        IntType -> "print_int"
                        FloatType -> "print_real"
        putCode ("    call_builtin " ++ func ++ "\n")

-- Write string statement
compileStmt (SWrite pos string)
    = do
        reg <- nextAvailableReg
        putCode ("    string_const r" ++ show reg ++ ", \"" ++ string ++ "\"" ++ "\n")
        putCode ("    call_builtin print_string" ++ "\n")

-- Call statement
compileStmt (Call pos ident es)
    = do
        proc <- getProcdure ident pos
        if (length proc) == (length es)
            then 
                do
                    compileExprs ident 0 es
                    putCode ("    call proc_" ++ ident ++ "\n")
            else error $ putPosition pos ++ "procedure call arity dose not matched \n"

-- if then statement
compileStmt (If pos expr stmts [])
    = do
        afterThen <- nextAvailableLabel
        exprType <- compileExpr 0 expr
        if exprType == BoolType 
            then
                do
                    putCode ("    branch_on_false r0, label_" ++ show(afterThen) ++ "\n")
                    compileStmts stmts
                    putStmtLabel afterThen
        else
            error $ putPosition pos ++ "Expression of If statement can not have type " ++ show(exprType)

-- if then else statement
compileStmt (If pos expr thenStmts elseStmts)
    = do
        inElse <- nextAvailableLabel
        afterElse <- nextAvailableLabel
        exprType <- compileExpr 0 expr
        if exprType == BoolType 
            then
                do
                    putCode ("    branch_on_false r0, label_" ++ show(inElse) ++ "\n")
                    compileStmts thenStmts
                    putCode ("    branch_uncond label_" ++ show(afterElse) ++ "\n")
                    putStmtLabel inElse
                    compileStmts elseStmts
                    putStmtLabel afterElse
        else
            error $ putPosition pos ++ "Expression of If statement can not have type " ++ show(exprType)

-- while statement
compileStmt (While pos expr stmts)
    = do
        inWhile <- nextAvailableLabel
        afterWhile <- nextAvailableLabel
        putStmtLabel inWhile
        exprType <- compileExpr 0 expr
        if exprType == BoolType 
            then
                do
                    putCode ("    branch_on_false r0, label_" ++ show afterWhile ++ "\n")
                    compileStmts stmts
                    putCode ("    branch_uncond label_" ++ show inWhile ++ "\n")
                    putStmtLabel afterWhile
        else
            error $ putPosition pos ++ "Expression of While statement can not have type " ++ show(exprType)

compileExprs :: String -> Int -> [Expr] -> State SymTable ()
compileExprs _ _ [] = return ()
compileExprs ident n (e:es)
    = do
        let pos = getExprPos e
        (isVal, baseType) <- getProcParameter ident n
        if isVal 
            then
                do
                    exprType <- compileExpr n e
                    if exprType == baseType 
                        then compileExprs ident (n+1) es
                        else error $ putPosition pos ++ " procedure parameter dose not match "

            else
                case e of
                    (Id _ stmtVar) -> do
                        stmtVarType <- getStmtVarBaseType stmtVar pos
                        if stmtVarType == baseType
                            then 
                                do
                                    storeAddressToRegN stmtVar n pos
                                    compileExprs ident (n+1) es
                            else error $ putPosition pos ++ " procedure parameter dose not match "
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
        if not isVal
            then putCode $ "    load r" ++ show destReg ++ ", " ++ show slot ++ "\n"
            else putCode $ "    load_address r" ++ show destReg ++ ", " ++ show slot ++ "\n"

storeAddressToRegN (IndexVar ident (IArray expr)) destReg pos
    = do
        (isVal, baseType, varShape, slot) <- getVariable ident (Array 0) pos
        offsetReg <- nextAvailableReg
        exprType <- compileExpr offsetReg expr
        if exprType == IntType 
            then putStoreAddressCodeOffset offsetReg slot destReg
            else error $ putPosition pos ++ " array index is not Int " ++ ident

storeAddressToRegN (IndexVar ident (IMatrix expr1 expr2)) destReg pos
    = do
        (isVal, baseType, (Matrix row col), slot) <- getVariable ident (Matrix 0 0) pos
        offsetReg <- nextAvailableReg
        colReg <- nextAvailableReg
        expr1Type <- compileExpr offsetReg expr1
        expr2Type <- compileExpr colReg expr2
        if expr1Type == IntType && expr2Type == IntType
            then 
                do
                    putSetOffsetReg offsetReg colReg col
                    putStoreAddressCodeOffset offsetReg slot destReg
            else error $ putPosition pos ++ " matrix index is not Int " ++ ident


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
                    slot <- nextAvailableSlot
                    insertVariable ident (True, baseType, Single, slot) pos
                    putCode $ "    store " ++ show slot ++ ", r" ++ show r ++ "         # " ++ ident ++ "\n"

            (ShapeVar ident shape)
                -> case shape of
                    (SArray num)
                        -> do
                            slot <- nextAvailableSlot
                            insertVariable ident (True, baseType, (Array num), slot) pos
                            putCode $ "    store " ++ show slot ++ ", r" ++ show r ++ "         # " ++ ident ++ "[" ++ show num ++ "]" ++ "\n"
                            putFilledSkipSlot r (num - 1)
                    (SMatrix row col)
                        -> do
                            slot <- nextAvailableSlot
                            insertVariable ident (True, baseType, (Matrix row col), slot) pos
                            putCode $ "    store " ++ show slot ++ ", r" ++ show r ++ "         # " ++ ident ++ "[" ++ show row ++ "," ++ show col ++ "]" ++ "\n"
                            putFilledSkipSlot r (row * col - 1)

putFilledSkipSlot :: Int -> Int -> State SymTable ()
putFilledSkipSlot _ 0 = return ()
putFilledSkipSlot r num 
    = do
        slot <- nextAvailableSlot
        putCode $ "    store " ++ show slot ++ ", r" ++ show r ++ "\n"
        putFilledSkipSlot r (num - 1)




whichDeclReg :: Int -> Int -> Decl -> Int
whichDeclReg ri rf (Decl _ baseType _)
    = if baseType == FloatType
        then rf
        else ri

----------- Expression Helper -----------

compileExpr :: Int -> Expr -> State SymTable BaseType
compileExpr reg (BoolConst _ b)
    = do
        let boolint = if b then 1 else 0
        putCode ("    int_const r" ++ show reg ++ ", " ++ show boolint ++ "\n")
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
compileExpr reg (Or pos expr1 expr2)           = compileLogicalExpr "or" reg expr1 expr2 pos
compileExpr reg (And pos expr1 expr2)          = compileLogicalExpr "and" reg expr1 expr2 pos
compileExpr reg (Less pos expr1 expr2)         = compileCompareExpr "lt" reg expr1 expr2 pos
compileExpr reg (LessEqual pos expr1 expr2)    = compileCompareExpr "le" reg expr1 expr2 pos
compileExpr reg (Greater pos expr1 expr2)      = compileCompareExpr "gt" reg expr1 expr2 pos
compileExpr reg (GreaterEqual pos expr1 expr2) = compileCompareExpr "ge" reg expr1 expr2 pos

compileExpr reg (Neg pos expr)
    = do
        type1 <- compileExpr reg expr
        if type1 == BoolType
            then
                do
                    putCode ("    not r" ++ show reg ++ ", r" ++ show reg ++ "\n")
                    return BoolType
        else
            error $ putPosition pos ++ "Can not negate type " ++ show type1

compileExpr reg (UMinus pos expr)
    = do
        type1 <- compileExpr reg expr
        if type1 == IntType || type1 == FloatType
            then
                do
                    putCode ("    not r" ++ show reg ++ ", r" ++ show reg ++ "\n")
                    return type1
        else
            error $ putPosition pos ++ "Can not negate type " ++ show type1

compileExpr reg (Id pos (SBaseVar ident))
    = do
        (isVal, baseType, varShape, slotnum) <- getVariable ident (Single) pos
        if varShape == Single then
            if isVal
                then
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
        exprType <- compileExpr (reg+1) expr
        case varShape of
            (Array n)
                -> do
                      if exprType == IntType
                          then
                              do
                                  putCode ("    load_address r" ++ show reg ++ ", " ++ show slotnum ++ "\n")
                                  putCode ("    sub_offset r" ++ show reg ++ ", r" ++ show reg ++ ", r" ++ show (reg+1) ++ "\n")
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
        type1 <- compileExpr (reg+1) expr1
        type2 <- compileExpr (reg+2) expr2
        case varShape of
            (Matrix a b)
                -> do
                      if type1 == IntType && type2 == IntType
                          then
                              do
                                  putCode $ "    int_const r" ++ show (reg+3) ++ ", " ++ show b ++ "\n"
                                  putCode $ "    mul_int r" ++ show (reg+3) ++ ", r" ++ show (reg+3) ++ ", r" ++ show (reg+1) ++ "\n"
                                  putCode $ "    add_int r" ++ show (reg+3) ++ ", r" ++ show (reg+3) ++ ", r" ++ show (reg+2) ++ "\n"
                                  putCode $ "    load_address r" ++ show reg ++ ", " ++ show slotnum ++ "\n"
                                  putCode $ "    sub_offset r" ++ show reg ++ ", r" ++ show reg ++ ", r" ++ show (reg+3) ++ "\n"
                                  putCode $ "    load_indirect r" ++ show reg ++ ", r" ++ show reg ++ "\n"
                                  return baseType
                      else
                          error $ putPosition pos ++ "Array Index must be IntType, while type " ++ show type1 ++ " and type " ++ show type2 ++ " received"
            (Single)
                -> error $ putPosition pos ++ "Expect Single expression, while Matrix expression is given"
            (Array _)
                -> error $ putPosition pos ++ "Expect Array expression, while Matrix expression is given"

compileArithmetricExpr :: String -> Int -> Expr -> Expr -> SourcePos -> State SymTable BaseType
compileArithmetricExpr s reg expr1 expr2 pos
    = do
        type1 <- compileExpr reg expr1
        type2 <- compileExpr (reg+1) expr2
        if type1 == type2 then
            if type1 == IntType
                then
                    do
                        putCode ("    " ++ s ++ "_int r" ++ show reg ++ ", r" ++ show reg ++ ", r" ++ show (reg+1) ++ "\n")
                        return IntType
            else
                if type1 == FloatType
                    then
                        do
                            putCode ("    " ++ s ++ "_real r" ++ show reg ++ ", r" ++ show reg ++ ", r" ++ show (reg+1) ++ "\n")
                            return FloatType
                else
                    error $ putPosition pos ++ "Can not " ++ s ++ " type " ++ show type1 ++ " with type " ++ show type2
        else
            if type1 == IntType && type2 == FloatType
                then
                    do
                        putCode ("    int_to_real r" ++ show reg ++ ", r" ++ show reg ++ "\n")
                        putCode ("    " ++ s ++ "_real r" ++ show reg ++ ", r" ++ show reg ++ ", r" ++ show (reg+1) ++ "\n")
                        return FloatType
            else
                if type1 == FloatType && type2 == IntType
                    then
                        do
                            putCode ("    int_to_real r" ++ show (reg+1) ++ ", r" ++ show (reg+1) ++ "\n")
                            putCode ("    " ++ s ++ "_real r" ++ show reg ++ ", r" ++ show reg ++ ", r" ++ show (reg+1) ++ "\n")
                            return FloatType
                    else
                        error $ putPosition pos ++ "Can not " ++ s ++ " type " ++ show type1 ++ " with type " ++ show type2

compileEqualityExpr :: String -> Int -> Expr -> Expr -> SourcePos -> State SymTable BaseType
compileEqualityExpr s reg expr1 expr2 pos
    = do
        type1 <- compileExpr reg expr1
        type2 <- compileExpr (reg+1) expr2
        if type1 == type2 then
            if type1 == FloatType
                then
                    do
                        putCode ("    cmp_" ++ s ++ "_real" ++ " r" ++ show reg ++ ", r" ++ show reg ++ ", r" ++ show (reg+1) ++ "\n")
                        return BoolType
            else
                do
                    putCode ("    cmp_" ++ s ++ "_int" ++ " r" ++ show reg ++ ", r" ++ show reg ++ ", r" ++ show (reg+1) ++ "\n")
                    return BoolType
        else
            error $ putPosition pos ++ "Can not compare " ++ s ++ " with type " ++ show type1 ++ " and type " ++ show type2

compileLogicalExpr :: String -> Int -> Expr -> Expr -> SourcePos -> State SymTable BaseType
compileLogicalExpr s reg expr1 expr2 pos
    = do
        type1 <- compileExpr reg expr1
        type2 <- compileExpr (reg+1) expr2
        if type1 == BoolType && type2 == BoolType
            then
                do
                    putCode ("    " ++ s ++ " r" ++ show reg ++ ", r" ++ show reg ++ ", r" ++ show (reg+1) ++ "\n")
                    return BoolType
        else
            error $ putPosition pos ++ s ++ " operation can not be used between type " ++ show type1 ++ " and " ++ show type2

compileCompareExpr :: String -> Int -> Expr -> Expr -> SourcePos -> State SymTable BaseType
compileCompareExpr s reg expr1 expr2 pos
    = do
        type1 <- compileExpr reg expr1
        type2 <- compileExpr (reg+1) expr2
        if type1 == type2 then
            if type1 == FloatType
                then
                    do
                        putCode ("    cmp_" ++ s ++ "_real r" ++ show reg ++ ", r" ++ show reg ++ ", r" ++ show (reg+1) ++ "\n")
                        return BoolType
            else
                do
                    putCode ("    cmp_" ++ s ++ "_int r" ++ show reg ++ ", r" ++ show reg ++ ", r" ++ show (reg+1) ++ "\n")
                    return BoolType
        else
            if type1 == IntType && type2 == FloatType
                then
                    do
                        putCode ("    int_to_real r" ++ show reg ++ ", r" ++ show reg ++ "\n")
                        putCode ("    cmp_" ++ s ++ "_real r" ++ show reg ++ ", r" ++ show reg ++ ", r" ++ show (reg+1) ++ "\n")
                        return BoolType
            else
                if type1 == FloatType && type2 == IntType
                    then
                        do
                            putCode ("    int_to_real r" ++ show (reg+1) ++ ", r" ++ show (reg+1) ++ "\n")
                            putCode ("    cmp_" ++ s ++ "_real r" ++ show reg ++ ", r" ++ show reg ++ ", r" ++ show (reg+1) ++ "\n")
                            return BoolType
                    else
                        error $ putPosition pos ++ "Can not compare" ++ s ++ " with type " ++ show type1 ++ " and type " ++ show type2

----------- Parameters Helper -----------

putParameters :: [Param] -> State SymTable ()
putParameters [] = return ()
putParameters (p:ps)
    = do
        putParameter p
        putParameters ps


putParameter :: Param -> State SymTable ()
putParameter (Param pos indicator baseType ident)
    = do
        slot <- nextAvailableSlot
        reg <- nextAvailableReg
        insertVariable ident ((indicator == Val), baseType, Single, slot) pos
        putCode $ "    store " ++ show slot ++ ", r" ++ show reg ++ "         # " ++ show indicator ++ " " ++ ident ++ "\n"


------------------ Procedure Helper ----------------------


putProcedureLabel :: String -> State SymTable ()
putProcedureLabel ident
    = do
        putCode $ "proc_" ++ ident ++ ":\n"

putProcedurePrologue :: [Param] -> [Decl] -> State SymTable Int
putProcedurePrologue params decls
    = do
        putComments "Prologue"
        let ps = length params
        let ds = getDeclsSize decls
        putCode $ "    push_stack_frame " ++ show (ps + ds) ++ "\n"
        return (ps + ds)

putProcedureEpilogue :: Int -> State SymTable ()
putProcedureEpilogue n
    = do
        putComments "Epilogue"
        putCode $ "    pop_stack_frame " ++ show n ++ "\n" ++ "    return\n"
        return ()


getDeclsSize :: [Decl] -> Int
getDeclsSize [] = 0
getDeclsSize (d:ds) = (getDeclSize d) + (getDeclsSize ds)

getDeclSize :: Decl -> Int
getDeclSize (Decl _ _ declVar) =
    case declVar of
        (DBaseVar _)
            -> 1
        (ShapeVar _ shape)
            -> case shape of
                    (SArray n)
                        -> n
                    (SMatrix n m)
                        -> n * m

------------------ Program Helper ----------------------

putProcedures :: [Proc] -> State SymTable ()
putProcedures [] = return ()
putProcedures (p:ps)
    = do
        putProcedure p
        putProcedures ps


putProcedure :: Proc -> State SymTable ()
putProcedure (Proc pos ident params _ _)
    = do
        let types = map (\(Param _ i bt _) -> ((i == Val), bt)) params
        insertProcedure ident types pos



checkMain :: State SymTable ()
checkMain
    = do
        st <- get
        let main = Map.lookup "main" (procedures st)
        case main of
            Nothing
                -> do
                    error $ "lack of main procedure"
            Just params
                -> do
                    if (length params) /= 0
                        then error $ "parameters in main procedure"
                        else return ()






test :: GoatProg -> ((), SymTable)
test prog  = runState (compileProg prog) (SymTable { labelCounter = 0, slotCounter = 0, regCounter = 0, code = "", procedures = Map.empty, variables = Map.empty })

getCode :: ((), SymTable) -> String
getCode (_, st) = code st
