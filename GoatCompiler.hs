import Control.Monad.State  
import qualified Data.Map as Map  

data SymTable = SymTable 
    { labelCounter :: Int
    , slotCounter  :: Int
    , regCounter   :: Int
    , code         :: String
    , variables    :: Map String (BaseType, Int)
    , procedure    :: Map String ([BaseType, Int])
    } deriving (Show)



nextAvaliableSlot :: State SymTable Int
nextAvaliableSlot = do
    symtable <- get
    let slot = slotCounter symtable
    put $ symtable {slotCounter = (slot + 1)}
    return slot


compile
            




test :: (Int, SymTable)
test = runState nextAvaliableSlot (SymTable {labelCounter = 0, slotCounter = 0, regCounter = 0})