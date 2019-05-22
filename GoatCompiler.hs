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

----------- Statement Helper -----------

putStatements :: [Stmt] -> State SymTable ()
putStatements s:ss
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
-- if then statement
compileStmt (If expr stmts [])
    = do
        afterThen <- nextAvailableLabel
        let exprType = compileExpr 0 expr
        if exprType == BoolType then
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
        let exprType = compileExpr 0 expr
        if exprType == BoolType then
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
        let exprType = compileExpr 0 expr
        if exprType == BoolType then
            putCode ("    branch_on_false r0, label_" ++ show(afterWhile) ++ "\n")
            compileStmts stmts
            putCode ("    branch_uncond label_" ++ show(inWhile) ++ "\n")
            putStmtLabel afterWhile
        else
            error $ "Expression of While statement can not have type " ++ show(exprType)

compileStmt (Call ident exprs)
    = do
        return ()

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
compileExpr a (Add expr1 expr2)
    = do
        let type1 = compileExpr a expr1
        let type2 = compileExpr (a+1) expr2
        if type1 == type2 then
            if type1 == IntType then
                putCode ("    add_int r" ++ show(a) ++ ", r" ++ show(a) ++ ", r" ++ show(a+1) ++ "\n")
                return IntType
            else
                if type1 == FloatType then
                    putCode ("    add_real r" ++ show(a) ++ ", r" ++ show(a) ++ ", r" ++ show(a+1) ++ "\n")
                    return FloatType
                else
                    error $ "Can not add type " ++ show(type1) ++ " with type " ++ show(type2)
        else
            if type1 == IntType and type2 == FloatType then
                putCode ("    int_to_real r" ++ show(a) ++ ", r" ++ show(a) ++ "\n")
                return FloatType
            else
                if type1 == FloatType and type2 == IntType then
                    putCode ("    int_to_real r" ++ show(a+1) ++ ", r" ++ show(a+1) ++ "\n")
                    return FloatType
                else
                    error $ "Can not minus " ++ show(type1) ++ " with " ++ show(type2)

compileExpr a (Minus expr1 expr2) = FloatType
compileExpr a (Mul expr1 expr2) = FloatType
compileExpr a (Div expr1 expr2) = FloatType
compileExpr a (Or expr1 expr2) = BoolType
compileExpr a (And expr1 expr2) = BoolType
compileExpr a (Equal expr1 expr2) = BoolType
compileExpr a (NotEqual expr1 expr2) = BoolType
compileExpr a (Less expr1 expr2) = BoolType
compileExpr a (LessEqual expr1 expr2) = BoolType
compileExpr a (Greater expr1 expr2) = BoolType
compileExpr a (GreaterEqual expr1 expr2) = BoolType
compileExpr a (Neg expr) == BoolType
compileExpr a (UMinus expr) == FloatType

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
