module Process where

import Data.SortedList
import Control.Monad

data Process = Process {
    name :: String,
    arrivalTime :: Int,
    requiredMemory :: Int,
    executionTime :: Int
} deriving (Eq, Show)

data ProcessorState = ProcessorState {
    processes :: SortedList ProcessInMemory,
    queue :: SortedList Process
} deriving (Show)

type SimState = (Int, ProcessorState, SortedList Process)

initialState :: ProcessorState
initialState = ProcessorState (toSortedList []) (toSortedList [])

newtype MemoryAddress = MemoryAddress { m :: Int }
    deriving (Eq, Ord)

instance Show MemoryAddress where
    show (MemoryAddress a) = show a

data ProcessInMemory = ProcessInMemory {
    address :: MemoryAddress,
    executionStart :: Int,
    process :: Process
} deriving (Eq, Show)

size :: ProcessInMemory -> Int
size (ProcessInMemory _ _ p) = requiredMemory p

end :: ProcessInMemory -> MemoryAddress
end = MemoryAddress . ap ((+) . m . address) (requiredMemory . process)

instance Ord Process where
  Process _ a _ _ `compare` Process _ b  _ _ = a `compare` b

instance Ord ProcessInMemory where
  ProcessInMemory a _ _ `compare` ProcessInMemory b _ _ = a `compare` b