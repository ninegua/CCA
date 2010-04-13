> {-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

> module Control.CCA 
>   ((>>>), (<<<), first, second, (***), (&&&), loop, 
>    Arrow, ArrowLoop, ArrowInit, 
>    arr, init, arr', init', constant,
>    norm, normOpt) where

> import Control.Arrow hiding (arr, returnA)
> import Control.CCA.Types hiding (init)
> import Control.CCA.CCNF
> import Language.Haskell.TH
> import Language.Haskell.TH.Syntax
> import Language.Haskell.TH.Instances
> import Prelude hiding (init)

> arr :: ExpQ -> ExpQ
> arr e = appE [|arr' e|] e

> init :: ExpQ -> ExpQ
> init i = appE [|init' i|] i

> constant :: (ArrowInit a, Lift b) => b -> a () b
> constant c = arr' [|const c|] (const c)

