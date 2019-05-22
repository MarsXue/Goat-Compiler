module GoatCompiler (test, getCode) where

import Control.Monad.State
import Data.Map (
    Map,
    (!),
    )
import qualified Data.Map as Map
import GoatAST

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
    , procedures   :: Map String ([BaseType])
    } deriving (Show)

-----------SymTable Helper-----------

skipSlot :: Int -> State SymTable ()
skipSlot n
    = do
        st <- get
        let slot = slotCounter st
        put $ st {slotCounter = (slot + n)}


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

insertProcedure :: String -> [BaseType] -> State SymTable ()
insertProcedure ident types
    = do
        st <- get
        if Map.member ident (procedures st)
            then error $ "duplicated procedure " ++ ident
            else put $ st { procedures = Map.insert ident types (procedures st) }


insertVariable :: String -> (Bool, BaseType, VarShape, Int) -> State SymTable ()
insertVariable ident (isVal, bt, vs, slot)
    = do
        st <- get
        if Map.member ident (variables st)
            then error $ "duplicate variable name " ++ ident
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

getVariable :: String -> State SymTable (Bool, BaseType, VarShape, Int)
getVariable ident
    = do
        st <- get
        case Map.lookup ident (variables st) of
            Nothing -> error $ "undefined variable " ++ ident
            Just v -> return v

-----------Main Compiler-----------

compileProg :: GoatProg -> State SymTable ()
compileProg (Prog ps)
    = do
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
compileProcedure (Proc ident params decls stmts)
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
        -- putComments "Compile Statements"
        -- putStatements stmts
        -- resetReg

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

compileStmt :: Stmt -> State SymTable ()
-- -- Assign statement
-- compileStmt (Assign stmtVar expr)
--     = do
--         compileExpr 0 expr
--         (_, _, _, slot) <- getVariable stmtVar
--         -- store 0, r0
--         putCode ("    store " ++ show slot ++ ", r0\n")

-- Read statement
compileStmt (Read (SBaseVar ident))
    = do
        putCode ("    call_builtin ")
        (_, ltype, _, slot) <- getVariable (show ident)
        case ltype of
            BoolType
                -> do
                    putCode ("read_bool\n")
            IntType
                -> do
                    putCode ("read_int\n")
            FloatType
                -> do
                    putCode ("read_real\n")

-- Write statement
compileStmt (Write expr)
    = do
        baseType <- compileExpr 0 expr
        let func = case baseType of
                        BoolType -> "print_bool"
                        IntType -> "print_int"
                        FloatType -> "print_real"
        (_, ltype, _, lslot) <- getVariable (show expr)
        putCode ("    load r0, " ++ show lslot ++ "\n")
        putCode ("    call_builtin " ++ func ++ "\n")

-- Write string statement
compileStmt (SWrite string)
    = do
        putCode ("  # write string")
        putCode ("    string_const r0, " ++ string ++ "\n")
        putCode ("    call_builtin print_string" ++ "\n")

-- Call statement
compileStmt (Call ident (e:es))
    = do
        return ()

-- if then statement
compileStmt (If expr stmts [])
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
            error $ "Expression of If statement can not have type " ++ show(exprType)

-- if then else statement
compileStmt (If expr thenStmts elseStmts)
    = do
        inElse <- nextAvailableLabel
        afterElse <- nextAvailableLabel
        exprType <- compileExpr 0 expr
        if exprType == BoolType
            then
                do
                    putCode ("    branch_on_false r0, label_" ++ show(inElse) ++ "\n")
                    compileStmts thenStmts
                    putCode ("    branch_uncond r0, label_" ++ show(afterElse) ++ "\n")
                    putStmtLabel inElse
                    compileStmts elseStmts
                    putStmtLabel afterElse
        else
            error $ "Expression of If statement can not have type " ++ show(exprType)

-- while statement
compileStmt (While expr stmts)
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
            error $ "Expression of While statement can not have type " ++ show(exprType)

compileStmt _ = return ()


----------- Declartion Helper -----------

putDeclarations :: [Decl] -> State SymTable ()
putDeclarations [] = return ()
putDeclarations ds
    = do
        ri <- nextAvailableReg
        putCode $ "    int_const r" ++ show ri ++ ", 0\n"
        rf <- nextAvailableReg
        putCode $ "    real_const r" ++ show rf ++ ", 0\n"
        putDeclarations' ri rf ds


putDeclarations' :: Int -> Int -> [Decl] -> State SymTable ()
putDeclarations' _ _ [] = return ()
putDeclarations' ri rf (d:ds)
    = do
        let r = whichDeclReg ri rf d
        putDeclaration' r d
        putDeclarations' ri rf ds

putDeclaration' :: Int -> Decl -> State SymTable ()
putDeclaration' r (Decl baseType declVar)
    = do
        case declVar of
            (DBaseVar ident)
                -> do
                    slot <- nextAvailableSlot
                    insertVariable ident (True, baseType, Single, slot)
                    putCode $ "    store " ++ show slot ++ ", r" ++ show r ++ "         # " ++ ident ++ "\n"

            (ShapeVar ident shape)
                -> case shape of
                    (SArray num)
                        -> do
                            slot <- nextAvailableSlot
                            insertVariable ident (True, baseType, (Array num), slot)
                            putCode $ "    store " ++ show slot ++ ", r" ++ show r ++ "         # " ++ ident ++ "[" ++ show num ++ "]" ++ "\n"
                            skipSlot (num - 1)
                    (SMatrix row col)
                        -> do
                            slot <- nextAvailableSlot
                            insertVariable ident (True, baseType, (Matrix row col), slot)
                            putCode $ "    store " ++ show slot ++ ", r" ++ show r ++ "         # " ++ ident ++ "[" ++ show row ++ "," ++ show col ++ "]" ++ "\n"
                            skipSlot (row * col - 1)



whichDeclReg :: Int -> Int -> Decl -> Int
whichDeclReg ri rf (Decl baseType _)
    = if baseType == FloatType
        then rf
        else ri

----------- Expression Helper -----------
compileExpr :: Int -> Expr -> State SymTable BaseType
compileExpr reg (BoolConst b)
    = do
        let boolint = if b then 1 else 0
        putCode ("    int_const r" ++ show reg ++ ", " ++ show boolint ++ "\n")
        return BoolType

compileExpr reg (IntConst i)
    = do
        putCode ("    int_const r" ++ show reg ++ ", " ++ show i ++ "\n")
        return IntType

compileExpr reg (FloatConst f)
    = do
        putCode ("    real_const r" ++ show reg ++ ", " ++ show f ++ "\n")
        return FloatType

compileExpr reg (Add expr1 expr2)          = return (compileArithmetricExpr "add" reg expr1 expr2)
compileExpr reg (Minus expr1 expr2)        = return (compileArithmetricExpr "sub" reg expr1 expr2)
compileExpr reg (Mul expr1 expr2)          = return (compileArithmetricExpr "mul" reg expr1 expr2)
compileExpr reg (Div expr1 expr2)          = return (compileArithmetricExpr "div" reg expr1 expr2)
compileExpr reg (Equal expr1 expr2)        = return (compileEqualityExpr "eq" reg expr1 expr2)
compileExpr reg (NotEqual expr1 expr2)     = return (compileEqualityExpr "ne" reg expr1 expr2)
compileExpr reg (Or expr1 expr2)           = return (compileLogicalExpr "or" reg expr1 expr2)
compileExpr reg (And expr1 expr2)          = return (compileLogicalExpr "and" reg expr1 expr2)
compileExpr reg (Less expr1 expr2)         = return (compileCompareExpr "lt" reg expr1 expr2)
compileExpr reg (LessEqual expr1 expr2)    = return (compileCompareExpr "le" reg expr1 expr2)
compileExpr reg (Greater expr1 expr2)      = return (compileCompareExpr "gt" reg expr1 expr2)
compileExpr reg (GreaterEqual expr1 expr2) = return (compileCompareExpr "ge" reg expr1 expr2)

compileExpr reg (Neg expr)
    = do
        type1 <- expr
        if type1 == BoolType
            then
                do
                    putCode ("    not r" ++ show reg ++ ", r" ++ show reg ++ "\n")
                    return BoolType
        else
            error $ "Can not negate type " ++ show type1

compileExpr reg (UMinus expr)
    = do
        type1 <- expr
        if type1 == IntType || type1 == FloatType
            then
                do
                    putCode ("    not r" ++ show reg ++ ", r" ++ show reg ++ "\n")
                    return type1
        else
            error $ "Can not negate type " ++ show type1

compileExpr reg (Id (SBaseVar ident))
    = do
        (isVal, baseType, varShape, slotnum) <- getVariable ident
        if varShape == Single then
            if isVal
                then
                    do
                        putCode ("    load r" ++ show reg ++ ", r" ++ show slotnum ++ "\n")
                        return baseType
            else
                do
                    putCode ("    load r" ++ show reg ++ ", r" ++ show slotnum ++ "\n")
                    putCode ("    load_indirect r" ++ show reg ++ ", r" ++ show reg ++ "\n")
                    return baseType
        else
            error $ "Expected type " ++ show varShape ++ ", while type Single received"

compileExpr reg (Id (IndexVar ident (IArray expr)))
    = do
        (_, baseType, varShape, slotnum) <- getVariable ident
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
                          error $ "Array Index must be IntType, while type " ++ show exprType ++ " received"
            Single
                -> error $ "Expect Single expression, while Array expression is given"
            (Matrix _ _)
                -> error $ "Expect Matrix expression, while Array expression is given"

compileExpr reg (Id (IndexVar ident (IArray expr1 expr2)))
    = do
        (_, baseType, varShape, slotnum) <- getVariable ident
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
                                  putCode $ "    sub_offset r" ++ show reg ++ ", r" ++ show reg ++ show (reg+3) ++ "\n"
                                  return baseType
                      else
                          error $ "Array Index must be IntType, while type " ++ show type1 ++ " and type " ++ show type2 " received"
            (Single)
                -> error $ "Expect Single expression, while Matrix expression is given"
            (Array _)
                -> error $ "Expect Array expression, while Matrix expression is given"



putSetOffsetReg :: Int -> Int -> Int -> State SymTable ()
putSetOffsetReg offsetReg colReg col
    = do
        sizeReg <- nextAvailableReg
        putCode $ "int_const r" ++ show sizeReg ++ ", " ++ show col ++ "\n"
        putCode $ "mul_int r" ++ show offsetReg ++ ", r" ++ show offsetReg ++ ", r" ++ sizeReg ++ "\n"
        putCode $ "add_int r" ++ show offsetReg ++ ", r" ++ show offsetReg ++ ", r" ++ show colReg ++ "\n"

putAssignCodeOffset :: Int -> Int -> Int -> State SymTable ()
putAssignCodeOffset offsetReg startSlot reg
    = do
        addrReg <- nextAvailableReg
        putCode $ "    load_address r" ++ show addrReg ++ ", " ++ show startSlot ++ "\n"
        putCode $ "    sub_offset r" ++ show addrReg ++ ", r" ++ show addrReg ++ ", r" ++ show offsetReg ++ "\n"
        putCode $ "    store_indirect r" ++ show addrReg ++ ", r" ++ show reg ++ "\n"


compileArithmetricExpr :: String -> Int -> Expr -> Expr -> State SymTable BaseType
compileArithmetricExpr s reg expr1 expr2
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
                    error $ "Can not " ++ s ++ " type " ++ show type1 ++ " with type " ++ show type2
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
                        error $ "Can not " ++ s ++ " type " ++ show type1 ++ " with type " ++ show type2

compileEqualityExpr :: String -> Int -> Expr -> Expr -> State SymTable BaseType
compileEqualityExpr s reg expr1 expr2
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
            error $ "Can not compare " ++ s ++ " with type " ++ show type1 ++ " and type " ++ show type2

compileLogicalExpr :: String -> Int -> Expr -> Expr -> State SymTable BaseType
compileLogicalExpr s reg expr1 expr2
    = do
        type1 <- compileExpr reg expr1
        type2 <- compileExpr (reg+1) expr2
        if type1 == BoolType && type2 == BoolType
            then
                do
                    putCode ("    " ++ s ++ " r" ++ show reg ++ ", r" ++ show reg ++ ", r" ++ show (reg+1) ++ "\n")
                    return BoolType
        else
            error $ s ++ " operation can not be used between type " ++ show type1 " and " + show type2

compileCompareExpr :: String -> Int -> Expr -> Expr -> State SymTable BaseType
compileCompareExpr s reg expr1 expr2
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
                        error $ "Can not compare" ++ s ++ " with type " ++ show type1 ++ " and type " ++ show type2
----------- Parameters Helper -----------

putParameters :: [Param] -> State SymTable ()
putParameters [] = return ()
putParameters (p:ps)
    = do
        putParameter p
        putParameters ps


putParameter :: Param -> State SymTable ()
putParameter (Param indicator baseType ident)
    = do
        slot <- nextAvailableSlot
        reg <- nextAvailableReg
        insertVariable ident ((indicator == Val), baseType, Single, slot)
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
getDeclSize (Decl _ declVar) =
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
putProcedure (Proc ident params _ _)
    = do
        let types = map (\(Param _ bt _) -> bt) params
        insertProcedure ident types



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
