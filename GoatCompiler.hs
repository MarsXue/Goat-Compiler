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



nextAvaliableSlot :: State SymTable Int
nextAvaliableSlot 
    = do
        st <- get
        let slot = slotCounter st
        put $ st {slotCounter = (slot + 1)}
        return slot


compileProg :: GoatProg -> State SymTable ()
compileProg (Prog ps)
    = do
        putProcedures ps
        checkMain


checkMain :: State SymTable ()
checkMain
    = do
        st <- get
        if Map.member "main" (procedures st)
            then return ()
            else error $ "no main procedure "


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





test :: GoatProg -> ((), SymTable)
test prog  = runState (compileProg prog) (SymTable { labelCounter = 0, slotCounter = 0, regCounter = 0, procedures = Map.empty })