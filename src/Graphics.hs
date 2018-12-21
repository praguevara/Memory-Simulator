module Graphics where

import           Algorithm
import           Process
import           Parser
import           Graphics.Gloss.Data.Picture
import           Graphics.Gloss.Interface.Pure.Game
import qualified Data.SortedList               as S
import Control.Monad (join)
import Control.Arrow ((***))


cA :: Color
cA = makeColorI 217 83 79 255
cB :: Color
cB = makeColorI 249 249 249 255
cC :: Color
cC = makeColorI 91 192 222 255
cD :: Color
cD = makeColorI 92 184 92 255
cE :: Color
cE = makeColorI 66 139 202 255

displaySize :: (Int, Int)
displaySize = (1280, 720)

simulation :: (Bool, Int, [SimState], [SimState]) -> IO ()
simulation s@(_, speed, _, _) = play
  (InWindow "Memory Simulator" displaySize (320, 180))
  cE
  speed
  s
  (\(_, _, _, n : _) -> draw n)
  events
  (\_ (r, sp, ps, n : ns) ->
    if null ns || not r then (False, sp, ps, n : ns) else (r, sp, n : ps, ns)
  )

events
  :: Event
  -> (Bool, Int, [SimState], [SimState])
  -> (Bool, Int, [SimState], [SimState])
events (EventKey (SpecialKey KeySpace) Down _ _) (r, s, a, b) =
  (not r, s, a, b)
events (EventKey (SpecialKey KeyLeft) Down _ _) (_, s, a : as, bs) =
  (False, s, as, a : bs)
events (EventKey (SpecialKey KeyRight) Down _ _) (_, s, as, b : bs) =
  (False, s, b : as, bs)
events _ a = a

draw :: SimState -> Picture
draw s@(t, ProcessorState _ q, _) =
  (Translate 0 (-200) . Scale 0.9 0.9 . drawChunks) s
    <> (Translate 500 (-350) . drawStep) t
    <> (Translate (-630) 320 . drawQueue) q

drawQueue :: S.SortedList Process -> Picture
drawQueue q = if (null . S.fromSortedList) q then blank else (Color cB . Scale 0.15 0.15 )
       . mconcat
       $ Scale 1.5 1.5 (Text (concat ["Queue (", (show . length . S.fromSortedList) q, " element(s)):"]))
       : (   (\(i, n) -> Translate 0 ((i + 1.5) * (-150)) . Text $ show n)
         <$> zip [0 ..] (S.fromSortedList q)
         )

drawChunks :: SimState -> Picture
drawChunks (_, ProcessorState st _, _) =
    (\(rs, ts) -> mconcat rs <> mconcat ts)
    $ unzip ((\c@(Chunk (MemoryAddress a, _) _) -> mapPair (Translate
    ( (0.5 * (widthChunk c - fromIntegral (fst displaySize)))
    + fromIntegral (a * fst displaySize) / fromIntegral totalMemory
    )
    0) $ drawChunk c) <$> chunks st)
    where mapPair = join (***)

drawChunk :: Chunk -> (Picture, Picture)
drawChunk c@(Chunk (MemoryAddress a, s) n) =
  ( mconcat
    [ Color (if n == "hueco" then cC else cD)
      . rectangleSolid (widthChunk c) $ fromIntegral (snd displaySize) / 3
    , Color cA $ rectangleWire (widthChunk c) (fromIntegral $ snd displaySize `div` 3)
    ]
  , Color cB $ mconcat
    [ (Scale 0.2 0.2 . Text) (if n == "hueco" then "-" else n)
    ,( Translate 0 (-50) . Scale 0.1 0.1 .  Text)
      ("(" ++ show a ++ ", " ++ show s ++ ")")
    ]
  )

widthChunk :: Chunk -> Float
widthChunk (Chunk (MemoryAddress _, s) _) =
  fromIntegral (s * fst displaySize) / fromIntegral totalMemory

drawStep :: Int -> Picture
drawStep = Color cB . Scale 0.2 0.2 . Text . ("t = " ++) . show
