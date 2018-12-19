module Parser where

import           Process
import           Data.SortedList
import           Algorithm
import           Data.List

readProcess :: String -> Process
readProcess x = case words x of
  [n, a, b, c] -> Process n (read a) (read b) (read c)
  _ -> error "Error reading process"

readProcesses :: FilePath -> IO [Process]
readProcesses = (((readProcess <$>) . lines) <$>) . readFile

writeState :: SimState -> String
writeState (t, ProcessorState ps _, _) =
  show t
    ++ " "
    ++ (   show
       <$> sort
       $   (processInMemoryToChunk <$> (fromSortedList ps))
       ++  (gapToChunk <$> (gaps ps))
       )

data Chunk = Chunk (MemoryAddress, Int) String deriving (Eq)

chunks :: SortedList ProcessInMemory -> [Chunk]
chunks ps =
  sort
    $  (processInMemoryToChunk <$> fromSortedList ps)
    ++ (gapToChunk <$> gaps ps)

processInMemoryToChunk :: ProcessInMemory -> Chunk
processInMemoryToChunk p = Chunk (address p, size p) (name (process p))

gapToChunk :: (MemoryAddress, Int) -> Chunk
gapToChunk = flip Chunk "hueco"

instance Show Chunk where
  show (Chunk (a, s) p) = concat ["[", show a, " ", show p, " " , show s, "]"]

instance Ord Chunk where
  compare (Chunk (a, _) _) (Chunk (b, _) _) = compare a b
