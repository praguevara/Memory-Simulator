module Graphics where

import           Algorithm
import           Process
import           Parser
import           Graphics.Gloss.Data.Picture
import           Graphics.Gloss.Interface.Pure.Game
import qualified Data.SortedList               as S


cA :: Color
cA = makeColorI 129 123 123 255
cB :: Color
cB = makeColorI 14 154 167 255
cC :: Color
cC = makeColorI 61 164 171 255
cD :: Color
cD = makeColorI 246 205 97 255
cE :: Color
cE = makeColorI 254 138 113 255

displaySize :: (Int, Int)
displaySize = (1280, 720)

simulation :: (Bool, Int, [SimState], [SimState]) -> IO ()
simulation s@(_, speed, _, _) = play
  (InWindow "Memory Simulator" displaySize (320, 180))
  cA
  speed
  s
  (\(_, _, _, n:_) -> draw n)
  events
  (\_ (r, sp, ps, n : ns) -> if null ns || not r then (False, sp, ps, n : ns) else (r, sp, n : ps, ns)
  )

events :: Event -> (Bool, Int, [SimState], [SimState]) -> (Bool, Int, [SimState], [SimState])
events (EventKey (SpecialKey KeySpace) Down _ _) (r, s, a, b) = (not r, s, a, b)
events (EventKey (SpecialKey KeyLeft) Down _ _) (r, s, a:as, bs) = (r, s, as, a:bs)
events (EventKey (SpecialKey KeyRight) Down _ _) (r, s, as, b:bs) = (r, s, b:as, bs)
events _ a = a

draw :: SimState -> Picture
draw s@(t, ProcessorState _ q, _) =
  drawChunks s
    <> (Translate 450 (-350) . drawStep) t
    <> ( (Color cD . Translate (-630) 340 . Scale 0.1 0.1)
       . mconcat
       $ Text "Queue:"
       : (   (\(i, n) -> Translate 0 ((i + 1.5) * (-150)) . Text $ show n)
         <$> zip [0 ..] (S.fromSortedList q)
         )
       )

drawChunks :: SimState -> Picture
drawChunks (_, ProcessorState st _, _) =
  Scale 0.9 0.9
    .   mconcat
    $   (\c@(Chunk (MemoryAddress a, _) _) ->
          ( Translate
                ( (0.5 * (widthChunk c - fromIntegral (fst displaySize)))
                + fromIntegral (a * fst displaySize `div` totalMemory)
                )
                0
            . drawChunk
            )
            c
        )
    <$> chunks st


drawChunk :: Chunk -> Picture
drawChunk c@(Chunk (MemoryAddress a, s) n) = mconcat
  [ Color (if n == "hueco" then cC else cB)
    $ rectangleSolid (widthChunk c) (fromIntegral $ snd displaySize)
  , Color cE $ rectangleWire (widthChunk c) (fromIntegral $ snd displaySize)
  , (Translate (-10) 0 . Scale 0.2 0.2 . Color cD . Text) n
  , (Translate (-10) (-30) . Scale 0.1 0.1 . Color cD . Text)
    ("(" ++ show a ++ ", " ++ show s ++ ")")
  ]

widthChunk :: Chunk -> Float
widthChunk (Chunk (MemoryAddress _, s) _) =
  fromIntegral (s * fst displaySize) / fromIntegral totalMemory

drawStep :: Int -> Picture
drawStep = Color cD . Scale 0.3 0.3 . Text . ("t = " ++) . show
