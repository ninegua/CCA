{-# LANGUAGE CPP, TemplateHaskell #-}

module Main where

#if __GLASGOW_HASKELL__ >= 610
import Control.Category
import Prelude hiding ((.), init, exp)
#else
import Prelude hiding (init, exp)
#endif

import Control.Arrow
import Control.CCA
import Control.CCA.CCNF
import Control.CCA.Types
import System.IO
import System.CPUTime
import Sample 
import qualified Sample1 as S

newtype SF a b = SF { runSF :: (a -> (b, SF a b)) }

#if __GLASGOW_HASKELL__ >= 610
instance Category SF where
  id = SF h where h x = (x, SF h)
  g . f = SF (h f g)
    where
      h f g x =
        let (y, f') = runSF f x
            (z, g') = runSF g y
        in (z, SF (h f' g'))
#endif

instance Arrow SF where
  arr f = g
    where g = SF (\x -> (f x, g))
#if __GLASGOW_HASKELL__ < 610
  f >>> g = SF (h f g)
    where
      h f g x = 
        let (y, f') = runSF f x
            (z, g') = runSF g y
        in (z, SF (h f' g'))
#endif
  first f = SF (g f)
    where
      g f (x, z) = ((y, z), SF (g f'))
        where (y, f') = runSF f x
  second f = SF (g f)
    where
      g f (z, x) = ((z, y), SF (g f'))
        where (y, f') = runSF f x
  f &&& g = SF (h f g)
    where
      h f g x =
        let (y, f') = runSF f x
            (z, g') = runSF g x 
        in ((y, z), SF (h f' g'))
  f *** g = SF (h f g)
    where
      h f g x =
        let (y, f') = runSF f (fst x)
            (z, g') = runSF g (snd x) 
        in ((y, z), SF (h f' g'))

instance ArrowLoop SF where
  loop sf = SF (g sf)
    where
      g f x = (y, SF (g f'))
        where ((y, z), f') = runSF f (x, z)

instance ArrowInit SF where
  init i = SF (f i)
    where f i x = (i, SF (f x))
  loopD i g = SF (f i) 
    where
      f i x = 
        let (y, i') = g (x, i)
        in (y, SF (f i'))

run :: SF a b -> [a] -> [b]
run (SF f) (x:xs) =
  let (y, f') = f x 
  in y `seq` (y : run f' xs)

unfold :: SF () a -> [a]
unfold = flip run inp
  where inp = () : inp

nth :: Int -> SF () a -> a
nth n (SF f) = x `seq` if n == 0 then x else nth (n - 1) f'
  where (x, f') = f ()

nth' :: Int -> (b, ((), b) -> (a, b)) -> a
nth' n (i, f) = aux n i
  where
    aux n i = x `seq` if n == 0 then x else aux (n-1) i'
      where (x, i') = f ((), i)
 
timer i = do t0 <- getCPUTime
             i `seq` (do
               t1 <- getCPUTime
	       let d = t1 - t0
               putStrLn $ show d ++ "\t" ++ show i
               return d)

gnuplot f l = do
      h <- openFile f WriteMode
      mapM_ (\(x, y) -> hPutStrLn h (show x ++ "\t" ++ show y)) 
            (zip [0, dt..] l)
      hClose h

plot3sec fn = gnuplot fn . take (sr * 3) . unfold 

testcase list = do
  ts <- mapM timer list
  let ts' = map (\x -> 1 / fromIntegral x) ts
  let x = minimum ts'
  let ns = map (/x) ts'
  sequence_ [ putStr (show (fromIntegral (floor (x * 100)) / 100) ++ "\t") | x <- ns ]
  putStrLn "\n"

main = do
  let n = 1000000
  putStrLn "Compare exp singal function"
  testcase [nth n S.exp, nth n exp, expNorm n, expOpt n]
  putStrLn "Compare sine singal function"
  testcase [nth n (S.sine 2), nth n (sine 2), sineNorm n, sineOpt n]
  putStrLn "Compare oscSine singal function"
  testcase [nth n (S.testOsc S.oscSine), nth n (testOsc oscSine), oscNorm n, oscOpt n]
  putStrLn "Compare sciFi singal function"
  testcase [nth n S.sciFi, nth n sciFi, sciFiNorm n, sciFiOpt n]
  putStrLn "Compare robot singal function"
  testcase [nth n (S.testRobot S.robot), nth n (testRobot robot), robotNorm n, robotOpt n]

expNorm n = nth n $(norm exp)
expOpt n = nth' n $(normOpt exp)

sineNorm n = nth n $(norm $ sine 2)
sineOpt n = nth' n $(normOpt $ sine 2)

oscNorm n = nth n $(norm $ testOsc oscSine)
oscOpt n = nth' n $(normOpt $ testOsc oscSine) 

sciFiNorm n = nth n $(norm sciFi)
sciFiOpt n = nth' n $(normOpt sciFi)

robotNorm n = nth n $(norm $ testRobot robot)
robotOpt n = nth' n $(normOpt $ testRobot robot)

