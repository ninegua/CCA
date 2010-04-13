{-# LANGUAGE Arrows #-}
module Sample1 where


import Control.Arrow
import Control.CCA.Types
import Prelude hiding (init, exp)

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

oscSine :: ArrowInit a => Double -> a Double Double
oscSine f0 = proc cv -> do
  let f = f0 * (2 ** cv)
  phi <- integral -< 2 * pi * f
  returnA -< sin phi

testOsc :: ArrowInit a => (Double -> a Double Double) -> a () Double
testOsc f = arr (const 1) >>> f 440

sciFi :: ArrowInit a => a () Double
sciFi = proc () -> do
   und <- oscSine 3.0 -< 0
   swp <- integral -< -0.25
   audio <- oscSine 440 -< und * 0.2 + swp + 1
   returnA -< audio

robot :: ArrowInit a => a (Double, Double) Double
robot = proc inp -> do
    let vr = snd inp
        vl = fst inp
        vz = vr + vl
    t <- integral -< vr - vl
    let t' = t / 10
    x <- integral -< vz * cos t'
    y <- integral -< vz * sin t'
    returnA -< x / 2 + y / 2

testRobot :: ArrowInit a => a (Double, Double) Double -> a () Double
testRobot bot = proc () -> do
    u <- sine 2 -< ()
    robot -< (u, 1 - u)

