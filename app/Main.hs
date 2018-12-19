import           Prelude                 hiding ( drop
                                                , span
                                                )
import           System.Environment
import           Algorithm
import           Parser
import           Process
import qualified Data.SortedList               as S
import           Graphics
import           System.Exit
import           Control.Monad.Writer
import           Control.Monad.Extra

main :: IO ()
main = do
  --[i] <- getArgs
  let i = "input.txt"
  ps  <- readProcesses i
  if null ps
    then do
      putStrLn "Call it with input file as argument"
      exitFailure
    else do
      f <- readFit
      c <- readCompact
      let (ss, written) =  steps (0 :: Int, initialState, S.toSortedList ps) f c
      g <- readPlaySimulation
      writeFile "output.txt"  (unlines $ writeState <$> ss)
      writeFile "output2.txt" (unlines $ show <$> written)
      when g $ simulation (True, 5, [], ss)

steps :: SimState
  -> Fit
  -> (Fit -> SimState -> Writer [Action] SimState)
  -> ([SimState], [Log])
steps s f c = runWriter $ take 10 <$> iterateM (step f c) s


takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore p = foldr (\x ys -> if p x then x:ys else [x]) []

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
