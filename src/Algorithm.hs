module Algorithm where

import           Prelude
import           Process
import           Data.List
import qualified Data.SortedList               as S
import           Data.Maybe
import           Control.Monad.Writer

totalMemory :: Int
totalMemory = 2000

type ActionSim = (Action, SimState)

data Action = ProcessIssued Process
            | ProcessInserted ProcessInMemory
            | ProcessRemoved ProcessInMemory
            | CompactedMemory (S.SortedList ProcessInMemory) (S.SortedList ProcessInMemory)
            | IncrementedTime Int
  deriving (Show)

gaps :: S.SortedList ProcessInMemory -> [(MemoryAddress, Int)]
gaps gs =
  catMaybes
    $   (\(a, b) ->
          if b - m a == 0 then Nothing else Just (MemoryAddress (m a), b - m a)
        )
    <$> zip (MemoryAddress 0 : (end <$> S.fromSortedList gs))
            ((m . address <$> S.fromSortedList gs) ++ [totalMemory])

type Fit = S.SortedList ProcessInMemory -> Process -> Maybe MemoryAddress

freeSpace :: S.SortedList ProcessInMemory -> Int
freeSpace ps = sum $ snd <$> gaps ps

compact :: S.SortedList ProcessInMemory -> Writer [Action] (S.SortedList ProcessInMemory)
compact ls = do
    let n = S.toSortedList $ scanl'
          (\a (ProcessInMemory _ t x) ->
            ProcessInMemory (MemoryAddress (m (end a))) t x
          )
          (ProcessInMemory (MemoryAddress 0) t0 px)
          ps
    tell [(CompactedMemory ls n)]
    return n
  where (ProcessInMemory _ t0 px : ps) = S.fromSortedList ls

shouldCompact :: S.SortedList ProcessInMemory -> Process -> Bool
shouldCompact ps p =
  freeSpace ps
    >= requiredMemory p
    && (snd . head . sortOn (\x -> (-1) * snd x) . gaps) ps
    <  requiredMemory p

insertProcess
  :: S.SortedList ProcessInMemory
  -> MemoryAddress
  -> Int
  -> Process
  -> Writer [Action] (S.SortedList ProcessInMemory)
insertProcess ps a t p = do
  tell [ProcessInserted (ProcessInMemory a t p)]
  return $ S.insert (ProcessInMemory a t p) ps

data Log = Log Int [(Action, SimState)]

instance Show Log where
  show (Log t as) = concat ["Instant t = ", show t, ":\n"] ++ concat (showAction <$> as)
    where showAction a = "\t" ++ show a ++ "\n"

step :: (SimState -> Writer [Action] SimState) -> SimState -> (SimState, Log)
step st s@(t, _, _) =
  let (a, a') = runWriter $ issue s
      (b, b') = runWriter $ removeFinishedProcesses a
      (c, c') = runWriter $ insertFromQueue st b
      (d, d') = runWriter $ incrementTime 1 c in
      (d, Log t $ concat $ expand <$> [(a', a), (b', b), (c', c), (d', d)])
    where expand (as, s') = (\a' -> (a', s')) <$> as

incrementTime :: Int -> SimState -> Writer [Action] SimState
incrementTime d (t, a, b) = do
  tell [IncrementedTime t]
  return (t + d, a, b)

-- reads processes to be issued and adds them to the queue
issue :: SimState -> Writer [Action] SimState
issue (t, s, ps) = do
  tell (ProcessIssued <$> S.fromSortedList a)
  return (t, ProcessorState (processes s) (S.union (queue s) a), b)
  where (a, b) = S.span ((t ==) . arrivalTime) ps

insertFromQueue
  :: (SimState -> Writer [Action] SimState)
  -> SimState
  -> Writer [Action] SimState
insertFromQueue x s@(_, ProcessorState _ psq, _) = case S.fromSortedList psq of
  [] -> return s
  _  -> x s

insertFromQueueNoCompact :: Fit -> SimState -> Writer [Action] SimState
insertFromQueueNoCompact fit s = do
  a <- insertFirstFromQueue fit s
  case a of
    Nothing -> return s
    Just x  -> insertFromQueueNoCompact fit x

-- 1. Add all possible from queue according to fit
-- 2. If should compact compact and go to 1
insertFromQueueCompact :: Fit -> SimState -> Writer [Action] SimState
insertFromQueueCompact fit s@(t, ProcessorState psm psq, tis) = do
  a <- insertFirstFromQueue fit s
  case a of
    Nothing -> if (not . null . S.fromSortedList) psq && shouldCompact psm (head $ S.fromSortedList psq)
      then insertFromQueueCompact fit (t, ProcessorState (compact psm) psq, tis)
      else return s
    Just x -> insertFromQueueCompact fit x

insertFirstFromQueue :: Fit -> SimState -> Writer [Action] (Maybe SimState)
insertFirstFromQueue fit (t, ProcessorState ps q, ts) = if null q
  then return Nothing
  else case fit ps x of
    Just i -> do
      a <- insertProcess ps i t x
      return $ Just (t, ProcessorState a xs, ts)
    Nothing -> return Nothing
 where
  x  = (head . S.fromSortedList) q
  xs = S.drop 1 q

removeFinishedProcesses :: SimState -> Writer [Action] SimState
removeFinishedProcesses (t, st, tis) = do
  tell $ ProcessRemoved <$> S.fromSortedList rms
  return (t, ProcessorState fps (queue st), tis)
 where
  (fps, rms) = S.partition
    (\x -> (t - executionStart x) < executionTime (process x))
    (processes st)

-- Fits
bestFit :: Fit
bestFit ps p =
  fst <$> find ((>= requiredMemory p) . snd) (sortOn snd (gaps ps))

worstFit :: Fit
worstFit ps p =
  fst <$> find ((>= requiredMemory p) . snd) (sortOn (((-1) *) . snd) (gaps ps))

firstFit :: Fit
firstFit ps p = fst <$> find ((>= requiredMemory p) . snd) (gaps ps)

availableFits :: [(String, Fit)]
availableFits =
  [("Best fit", bestFit), ("Worst fit", worstFit), ("First fit", firstFit)]
