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





test :: ((), SymTable)
test = runState (compileProg (Prog [Proc "g" [Param Val FloatType "x",Param Ref IntType "k"] [Decl IntType (DBaseVar "n"),Decl FloatType (DBaseVar "y"),Decl BoolType (ShapeVar "a" (SArray 8))] [Assign (IndexVar "a" (IArray (IntConst 7))) (BoolConst True),Assign (SBaseVar "k") (IntConst 42)],Proc "p" [Param Ref IntType "i"] [] [Assign (SBaseVar "i") (Minus (Add (Mul (IntConst 6) (Id (SBaseVar "i"))) (IntConst 4)) (Id (SBaseVar "i")))],Proc "main" [] [Decl IntType (DBaseVar "m"),Decl IntType (DBaseVar "n")] [Read (SBaseVar "n"),While (Greater (Id (SBaseVar "n")) (IntConst 1)) [Assign (SBaseVar "m") (Id (SBaseVar "n")),While (Greater (Id (SBaseVar "m")) (IntConst 0)) [If (Greater (Id (SBaseVar "m")) (IntConst 0)) [Assign (SBaseVar "n") (Minus (Id (SBaseVar "n")) (IntConst 1)),Assign (SBaseVar "m") (Minus (Id (SBaseVar "m")) (IntConst 1)),If (Equal (Id (SBaseVar "m")) (IntConst 0)) [Call "p" [Id (SBaseVar "n")]] []] [Assign (SBaseVar "m") (Minus (Id (SBaseVar "n")) (Id (SBaseVar "m"))),Assign (SBaseVar "m") (Minus (Id (SBaseVar "m")) (IntConst 1))]]]]])) (SymTable { labelCounter = 0, slotCounter = 0, regCounter = 0, procedures = Map.empty })