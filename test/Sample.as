module Sample where

import Control.CCA
import Prelude hiding (init, exp)
import Language.Haskell.TH

sr = 44100 :: Int
dt = 1 / (fromIntegral sr)

exp :: ArrowInit a => a () Double
exp = proc () -> do
  rec let e = 1 + i
      i <- integral -< e
  returnA -< e

integral :: ArrowInit a => a Double Double
integral = proc x -> do
  rec let i' = i + x * dt
      i <- init 0 -< i'
  returnA -< i

sineA :: ArrowInit a => Double -> a () Double
sineA freq = proc _ -> do
              rec 
               d2o <- init (sin omh) -< r
               d1o <- init 0 -< d2o
               let r = 2 * cos(omh) * d2o - d1o
              returnA -< r
  where 
    omh = 2*pi/(fromIntegral sr)*freq

sine :: ArrowInit a => Double -> a () Double
sine freq = proc _ -> do
  rec x <- init i -< r
      y <- init 0 -< x 
      let r = c * x - y
  returnA -< r
  where
    omh = 2 * pi / (fromIntegral sr) * freq
    i = sin omh
    c = 2 * cos omh

