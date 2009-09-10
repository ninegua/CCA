> {-# LANGUAGE CPP #-}

> module Control.CCA.Types where

#if __GLASGOW_HASKELL__ >= 610

> import Control.Category

#endif

> import Control.Arrow 
> import Language.Haskell.TH
> import Prelude hiding (init)

> class (Arrow a, ArrowLoop a) => ArrowInit a where
>   init :: b -> a b b
>   arr' :: ExpQ -> (b -> c) -> a b c
>   arr' _ = arr
>   init' :: ExpQ -> b -> a b b
>   init' _ = init
>   loopD :: e -> a (b, e) (c, e) -> a b c 
>   loopD i f = loop (f >>> second (init i))
>   loopB :: e -> a (b, (d, e)) (c, (d, e)) -> a b c
>   loopB i f = loop (f >>> second (second (init i)))


