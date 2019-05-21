module GoatCompiler (test) where

import Control.Monad.State
import Data.Map (
    Map,
    (!),
    )
import qualified Data.Map as Map
import GoatAST

data SymTable = SymTable
    { labelCounter :: Int
    , slotCounter  :: Int
    , regCounter   :: Int
    -- , code         :: String
    -- , variables    :: Map String (Decl, Int)
    , procedures   :: Map String ([BaseType])
    } deriving (Show)


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


compileProg :: GoatProg -> State SymTable ()
compileProg (Prog ps)
    = do
        nextAvailableLabel
        nextAvailableLabel
        nextAvailableLabel
        nextAvailableLabel
        putProcedures ps
        checkMain


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


putProcedures :: [Proc] -> State SymTable ()
putProcedures [] = return ()
putProcedures (p:ps)
    = do
        putProcedure p
        putProcedures ps

putProcedure :: Proc -> State SymTable ()
putProcedure (Proc ident params _ _)
    = do
        st <- get
        let types = map (\(Param _ bt _) -> bt) params
        if Map.member ident (procedures st)
            then error $ "duplicated procedure " ++ ident
            else put $ st { procedures = Map.insert ident types (procedures st) }
        return ()


-- compileProcedures :: [Proc] -> State SymTable ()
-- compileProcedures (p:ps)
--     = do
--         compileProcedure p
--         compileProcedures ps

-- compileProcedure :: Proc -> State SymTable ()
-- compileProcedure 


test :: GoatProg -> ((), SymTable)
test prog  = runState (compileProg prog) (SymTable { labelCounter = 0, slotCounter = 0, regCounter = 0, procedures = Map.empty })
