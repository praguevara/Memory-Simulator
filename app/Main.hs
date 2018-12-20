import           Prelude                 hiding ( drop
                                                , span
                                                )
import           Algorithm
import           Parser
import           Process
import qualified Data.SortedList               as S
import           Graphics
import           System.Exit
import           Control.Monad.Writer

main :: IO ()
main = do
  --[i] <- getArgs
  let i = "input.txt"
  ps <- readProcesses i
  if null ps
    then do
      putStrLn "Call it with input file as argument"
      exitFailure
    else do
      f <- readFit
      c <- readCompact
      let (ss, logs) =
            (unzip . steps (0 :: Int, initialState, S.toSortedList ps) . c) f
      g <- readPlaySimulation
      writeFile "output.txt"  (unlines (writeState <$> ss))
      writeFile "actions.txt" (unlines (show <$> filter (\(Log _ a) -> (not . null) a) logs))
      when g $ simulation (True, 5, [], ss)


steps :: SimState -> (SimState -> Writer [Action] SimState) -> [(SimState, Log)]
steps s c = takeWhileOneMore
  (\((_, ProcessorState psm psq, psi), _) ->
    not $ null psm && null psq && null psi
  )
  (iterate (step c . fst) (s, Log 0 []))


takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore p = foldr (\x ys -> if p x then x : ys else [x]) []

readFit :: IO Fit
readFit = do
  putStrLn "Choose fit:"
  mapM_ (\(i, (n, _)) -> putStrLn $ concat [show i, ": ", n])
    $ zip ([1 ..] :: [Int]) availableFits
  o <- getLine
  return $ snd $ availableFits !! (read o - 1)

readCompact :: IO (Fit -> SimState -> Writer [Action] SimState)
readCompact = do
  putStrLn "Do you want to enable compacting? [y/n]"
  r <- getLine
  return
    (if (head r == 'y') || (head r == 'Y')
      then insertFromQueueCompact
      else insertFromQueueNoCompact
    )

readPlaySimulation :: IO Bool
readPlaySimulation = do
  putStrLn "Do you want to play the simulation? [y/n]"
  (r : _) <- getLine
  return (r == 'y' || r == 'Y')
