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

insertProcedure :: String -> [(Bool, BaseType)] -> State SymTable ()
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

getProcdure :: String -> State SymTable ([(Bool, BaseType)])
getProcdure ident
    = do
        st <- get
        case Map.lookup ident (procedures st) of
            Nothing -> error $ "undefined procedure " ++ ident
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
        -- resetProcedure
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
        let name = sourceName pos
            line = sourceLine pos
            column = sourceColumn pos
        in
            show name ++ " Line " ++ show line ++ " Column " ++ show column ++ "\n"


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

getStmtVarBaseType :: StmtVar -> State SymTable BaseType
getStmtVarBaseType (SBaseVar ident)
    = do
        (isVal, baseType, varShape, slot) <- getVariable ident
        return baseType
getStmtVarBaseType (IndexVar ident index)
    = do
        (isVal, baseType, varShape, slot) <- getVariable ident
        return baseType

assginableType :: BaseType -> BaseType -> Bool
assginableType st ex = 
    if st == ex then True
        else
            if st == FloatType && ex == IntType then True
                else False

putAssignCode :: StmtVar -> Int -> State SymTable ()
putAssignCode (SBaseVar ident) reg
    = do
        (isVal, baseType, varShape, slot) <- getVariable ident
        if not isVal 
            then putAssignCodeRef slot reg
            else putCode $ "    store " ++ show slot ++ ", r" ++ show reg ++ "\n"

putAssignCode (IndexVar ident (IArray expr)) reg
    = do 
        (isVal, baseType, varShape, slot) <- getVariable ident
        offsetReg <- nextAvailableReg
        exprType <- compileExpr offsetReg expr
        if exprType == IntType 
            then putAssignCodeOffset offsetReg slot reg
            else error $ "array index is not Int"

putAssignCode (IndexVar ident (IMatrix expr1 expr2)) reg
    = do
        (isVal, baseType, (Matrix row col), slot) <- getVariable ident
        offsetReg <- nextAvailableReg
        colReg <- nextAvailableReg
        expr1Type <- compileExpr offsetReg expr1
        expr2Type <- compileExpr colReg expr2
        if expr1Type == IntType && expr2Type == IntType
            then 
                do
                    putSetOffsetReg offsetReg colReg col
                    putAssignCodeOffset offsetReg slot reg
            else error $ "array index is not Int"

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

compileStmt :: Stmt -> State SymTable ()
compileStmt (Assign _ stmtVar expr)
    = do
        regThis <- nextAvailableReg
        exprType <- compileExpr regThis expr
        stmtType <- getStmtVarBaseType stmtVar
        if assginableType stmtType exprType 
            then putAssignCode stmtVar regThis
            else error $ "assginment type dose not match" 
---------------------------------------------------------------------------------------------------------------------

-- Read statement
compileStmt (Read _ (SBaseVar ident))
    = do
        reg <- nextAvailableReg
        putCode ("    call_builtin ")
        (_, ltype, _, slot) <- getVariable ident
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
        putCode ("    store " ++ show slot ++ ", r" ++ show reg ++ "\n")

-- Write statement
compileStmt (Write _ expr)
    = do
        baseType <- compileExpr 0 expr
        let func = case baseType of
                        BoolType -> "print_bool"
                        IntType -> "print_int"
                        FloatType -> "print_real"
        putCode ("    call_builtin " ++ func ++ "\n")

-- Write string statement
compileStmt (SWrite _ string)
    = do
        reg <- nextAvailableReg
        putCode ("    string_const r" ++ show reg ++ ", \"" ++ string ++ "\"" ++ "\n")
        putCode ("    call_builtin print_string" ++ "\n")

-- Call statement
compileStmt (Call _ ident es)
    = do
        proc <- getProcdure ident
        if (length proc) == (length es)
            then 
                do
                    compileExprs ident 0 es
                    putCode ("    call proc_" ++ ident ++ "\n")
            else error $ "procedure call arity dose not matched \n"

-- if then statement
compileStmt (If _ expr stmts [])
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
compileStmt (If _ expr thenStmts elseStmts)
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
compileStmt (While _ expr stmts)
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

compileExprs :: String -> Int -> [Expr] -> State SymTable ()
compileExprs _ _ [] = return ()
compileExprs ident n (e:es)
    = do
        (isVar, baseType) <- getProcParameter ident n
        if isVar 
            then
                do
                    exprType <- compileExpr n e
                    if exprType == baseType 
                        then return ()
                        else error $ " procedure parameter dose not match "

            else
                case e of
                    (Id _ stmtVar) -> do
                        stmtVarType <- getStmtVarBaseType stmtVar
                        if stmtVarType == baseType
                            then storeAddressToRegN stmtVar n 
                            else error $ " procedure parameter dose not match "
                    _ -> error $ " Ref procedure parameter dose not allow Non-lvalue "

storeAddressToRegN :: StmtVar -> Int -> State SymTable ()
storeAddressToRegN (SBaseVar ident) destReg
    = do
        (isVal, baseType, varShape, slot) <- getVariable ident
        if not isVal
            then putCode $ "    load r" ++ show destReg ++ ", " ++ show slot ++ "\n"
            else putCode $ "    load_address r" ++ show destReg ++ ", " ++ show slot ++ "\n"

storeAddressToRegN (IndexVar ident (IArray expr)) destReg
    = do
        (isVal, baseType, varShape, slot) <- getVariable ident
        offsetReg <- nextAvailableReg
        exprType <- compileExpr offsetReg expr
        if exprType == IntType 
            then putStoreAddressCodeOffset offsetReg slot destReg
            else error $ " array index is not Int " ++ ident

storeAddressToRegN (IndexVar ident (IMatrix expr1 expr2)) destReg
    = do
        (isVal, baseType, (Matrix row col), slot) <- getVariable ident
        offsetReg <- nextAvailableReg
        colReg <- nextAvailableReg
        expr1Type <- compileExpr offsetReg expr1
        expr2Type <- compileExpr colReg expr2
        if expr1Type == IntType && expr2Type == IntType
            then 
                do
                    putSetOffsetReg offsetReg colReg col
                    putStoreAddressCodeOffset offsetReg slot destReg
            else error $ " matrix index is not Int " ++ ident


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
putDeclaration' r (Decl _ baseType declVar)
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

compileExpr a (Add _ expr1 expr2)
    = do
        type1 <- compileExpr a expr1
        type2 <- compileExpr (a+1) expr2
        if type1 == type2 then
            if type1 == IntType 
                then
                    do
                        putCode ("    add_int r" ++ show a ++ ", r" ++ show a ++ ", r" ++ show (a+1) ++ "\n")
                        return IntType
            else
                if type1 == FloatType 
                    then
                        do
                            putCode ("    add_real r" ++ show a ++ ", r" ++ show a ++ ", r" ++ show (a+1) ++ "\n")
                            return FloatType
                else
                    error $ "Can not add type " ++ show type1 ++ " with type " ++ show type2 ++ "\n"
        else
            if type1 == IntType && type2 == FloatType 
                then
                    do
                        putCode ("    int_to_real r" ++ show a ++ ", r" ++ show a ++ "\n")
                        return FloatType
            else
                if type1 == FloatType && type2 == IntType 
                    then
                        do
                            putCode $ "    int_to_real r" ++ show (a+1) ++ ", r" ++ show (a+1) ++ "\n"
                            return FloatType
                    else
                        error $ "Can not minus " ++ show type1 ++ " with " ++ show type2 ++ "\n"

compileExpr a (Minus _ expr1 expr2) = return FloatType
compileExpr a (Mul _ expr1 expr2) = return FloatType
compileExpr a (Div _ expr1 expr2) = return FloatType
compileExpr a (Or _ expr1 expr2) = return BoolType
compileExpr a (And _ expr1 expr2) = return BoolType
compileExpr a (Equal _ expr1 expr2) = return BoolType
compileExpr a (NotEqual _ expr1 expr2) = return BoolType
compileExpr a (Less _ expr1 expr2) = return BoolType
compileExpr a (LessEqual _ expr1 expr2) = return BoolType
compileExpr a (Greater _ expr1 expr2) = return BoolType
compileExpr a (GreaterEqual _ expr1 expr2) = return BoolType
compileExpr a (Neg _ expr) = return BoolType
compileExpr a (UMinus _ expr) = return FloatType
compileExpr a (Id _ b) = return IntType


----------- Parameters Helper -----------

putParameters :: [Param] -> State SymTable ()
putParameters [] = return ()
putParameters (p:ps)
    = do
        putParameter p
        putParameters ps


putParameter :: Param -> State SymTable ()
putParameter (Param _ indicator baseType ident)
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
putProcedure (Proc _ ident params _ _)
    = do
        let types = map (\(Param _ i bt _) -> ((i == Val), bt)) params
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
