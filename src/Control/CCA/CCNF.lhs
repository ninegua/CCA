> {-# LANGUAGE CPP, TemplateHaskell, FlexibleInstances #-}

> module Control.CCA.CCNF 
>   (norm, normOpt, 
>    pprNorm, pprNormOpt, printCCA, ASyn) where

#if __GLASGOW_HASKELL__ >= 610

> import Control.Category
> import Prelude hiding ((.), init)

#else

> import Prelude hiding (init)

#endif

> import Control.Arrow 
> import Control.CCA.Types
> import Data.Char (isAlpha)
> import Language.Haskell.TH
> import Language.Haskell.TH.Syntax
> import Language.Haskell.TH.Instances
> import qualified Data.Generics as G (everywhere, mkT)

Internal Representation
=======================

We use AExp to syntactically represent an arrow for normalization purposes. 

> data AExp
>   = Arr ExpQ
>   | First AExp
>   | AExp :>>> AExp
>   | Loop AExp
>   | LoopD ExpQ AExp    -- loop with initialized feedback
>   | LoopB ExpQ AExp    -- loop with both immediate and initialized feedback
>   | Init ExpQ
>   | Lft AExp

> infixl 1 :>>>

We use phantom types to make ASyn an Arrow.

> newtype ASyn b c = AExp AExp 

#if __GLASGOW_HASKELL__ >= 610

> instance Category ASyn where
>   id = AExp (Arr [|\x -> x|])
>   AExp g . AExp f = AExp (f :>>> g)

> instance Arrow ASyn where
>   arr f = error "use arr' instead" 
>   first (AExp f) = AExp (First f)

#else

> instance Arrow ASyn where
>   arr f = error "use arr' instead" 
>   AExp f >>> AExp g = AExp (f :>>> g)
>   first (AExp f) = AExp (First f)

#endif

> instance ArrowLoop ASyn where
>   loop (AExp f) = AExp (Loop f)

> instance ArrowInit ASyn where
>   init i = error "use init' instead"
>   arr' f _ = AExp (Arr f)
>   init' i _ = AExp (Init i)


ArrowChoice only requires definition for 'left', but the default implementation
for 'right' and '|||' uses arr so we need to redefine them using arr' here.
'+++' is also redefined here for completeness.

> instance ArrowChoice ASyn where
>   left (AExp f) = AExp (Lft f)
>   right f = arr' [| mirror |] mirror >>> left f
>             >>> arr' [| mirror |] mirror
>   f +++ g = left f >>> right g
>   f ||| g = f +++ g >>> arr' [| untag |] untag

These helpers have to be at the top-level because TH cannot splice in a
polymorphic local function.

> mirror (Left x) = Right x
> mirror (Right y) = Left y

> untag (Left x) = x
> untag (Right y) = y

Pretty printing AExp.

> printCCA (AExp x) = printAExp x
> printAExp x = runQ (fromAExp x) >>= putStrLn . simplify . pprint
> simplify = unwords . map (unwords . (map aux) . words) . lines 
>   where aux (c:x) | not (isAlpha c) = c : aux x
>         aux x = let (u, v) = break (=='.') x
>                 in if length v > 1 then aux (tail v)
>                                    else x

Traversal over AExp is defined in terms of imap (intermediate map) 
and everywhere. 

> type Traversal = AExp -> AExp
> imap :: Traversal -> Traversal
> imap h (First f) = First (h f)
> imap h (f :>>> g) = h f :>>> h g
> imap h (Loop f) = Loop (h f)
> imap h (LoopD i f) = LoopD i (h f)
> imap h (LoopB i f) = LoopB i (h f)
> imap h (Lft f) = Lft (h f)
> imap h x = x

> everywhere :: Traversal -> Traversal 
> everywhere h e = h (imap (everywhere h) e)

Normalization
=============

norm is a TH function that normalizes a given CCA, e.g., $(norm e) will
give the CCNF of e. 

> norm :: ASyn t t1 -> ExpQ         -- returns a generic ArrowInit arrow
> norm (AExp e) = fromAExp (normE e)
> normE = everywhere (normalize normE)

normOpt returns the pair of state and pure function as (i, f) from optimized 
CCNF in the form loopD i (arr f). 

> normOpt :: ASyn t t1 -> ExpQ      -- returns a pair of state and pure function (s, f)
> normOpt (AExp e) = 
>   case toLoopD $ normE e of
>     LoopD i (Arr f) -> tupE [i, f]
>     _         -> error "The given arrow can't be normalized to optimized CCNF." 

pprNorm and pprNormOpt return the pretty printed normal forms as a 
string.

> pprNorm = ppr' . norm
> pprNormOpt = ppr' . normOpt
> ppr' e = runQ (fmap toLet e) >>= litE . StringL . simplify . pprint

fromAExp converts AExp back to TH Exp structure.

> fromAExp (Arr f) = appE [|arr|] f
> fromAExp (First f) = appE [|first|] (fromAExp f)
> fromAExp (f :>>> g) = infixE (Just (fromAExp f)) [|(>>>)|] (Just (fromAExp g))
> fromAExp (Loop f) = appE [|loop|] (fromAExp f)
> fromAExp (LoopD i f) = appE (appE [|loopD|] i) (fromAExp f)
> fromAExp (LoopB i f) = appE (appE [|loopB|] i) (fromAExp f)
> fromAExp (Init i) = appE [|init|] i
> fromAExp (Lft f) = appE [|left|] (fromAExp f)

CCNF
====

Arrow laws:

> normalize norm (Arr f :>>> Arr g) = Arr (g `o` f)
> normalize norm (First (Arr f)) = Arr (f `crossE` idE)
> normalize norm (First f :>>> First g) = First ((f :>>> g))

Choice:

> normalize norm (Lft (Arr f)) = Arr [| \x -> case x of
>                                               Left a -> Left ($f a)
>                                               Right b -> Right b |]
> normalize norm (Lft (LoopB i f)) =
>     norm (LoopB i (Arr tagE :>>> Lft f :>>> Arr untagE))
>     where
>       tagE = [| \(inp,s) -> case inp of
>                               Left a -> Left (a, s)
>                               Right c -> Right (c, s) |]
>       untagE = [| \out -> case out of
>                             Left (b,s) -> (Left b, s)
>                             Right (c,s) -> (Right c, s) |]

LoopB laws:

> normalize norm (h :>>> (LoopB i f)) = norm (LoopB i (First h :>>> f))
> normalize norm ((LoopB i f) :>>> h) = norm (LoopB i (f :>>> First h))
> normalize norm (LoopB i (LoopB j f)) = 
>   norm (LoopB (tupE [i, j]) (Arr shuffleE :>>> f :>>> Arr shuffleE'))
>   where
>     shuffleE = assocE' `o` (idE `crossE` transposeE)
>     shuffleE' = (idE `crossE` transposeE) `o` assocE
> normalize norm (First (LoopB i f)) = 
>  norm (LoopB i (Arr juggleE :>>> First f :>>> Arr juggleE))

LoopD, Loop, and Init are translated to LoopB:

>-- normalize norm (LoopD i f) = norm (Loop (f :>>> secondA (Init i)))
>-- normalize norm (Loop f) = LoopB (Term "()") (norm (Arr assocE' :>>> First f :>>> Arr assocE))
>-- normalize norm (Init i) = LoopB i (Arr (swapE `o` juggleE `o` swapE))

LoopD laws

> normalize norm (h :>>> LoopD i f) = norm (LoopD i (First h :>>> f))
> normalize norm (LoopD i f :>>> h) = norm (LoopD i (f :>>> First h))
> normalize norm (LoopD i (LoopD j f)) = 
>   norm (LoopD (tupE [i, j]) (Arr assocE' :>>> f :>>> Arr assocE))
> normalize norm (First (LoopD i f)) = 
>   norm (LoopD i (Arr juggleE :>>> First f :>>> Arr juggleE))

Loop laws

> normalize norm (h :>>> Loop f) = norm (Loop (First h :>>> f))
> normalize norm (Loop f :>>> h) = norm (Loop (f :>>> First h))
> normalize norm (Loop (Loop f)) = 
>   norm (Loop (Arr assocE' :>>> f :>>> Arr assocE))
> normalize norm (First (Loop f)) = 
>   norm (Loop (Arr juggleE :>>> First f :>>> Arr juggleE))

Loop and LoopB

> normalize norm (Loop (LoopB i f)) = 
>   norm (LoopB i (Arr shuffleE :>>> f :>>> Arr shuffleE'))
>  where 
>    shuffleE = assocE' `o` (idE `crossE` assocE)
>    shuffleE' = (idE `crossE` assocE') `o` assocE

> normalize norm (LoopB i (Loop f)) = 
>   norm (LoopB i (Arr shuffleE :>>> f :>>> Arr shuffleE'))
>   where 
>     shuffleE = assocE' `o` (idE `crossE` juggleE)
>     shuffleE' = (idE `crossE` juggleE) `o` assocE

Loop and LoopD

> normalize norm (Loop (LoopD i f)) = 
>   norm (LoopB i (Arr assocE' :>>> f :>>> Arr assocE))

> normalize norm (LoopD i (Loop f)) = 
>   norm (LoopB i (Arr (juggleE `o` assocE') :>>> f :>>> 
>                  Arr (assocE' `o` juggleE)))

LoopB AND LoopD

> normalize norm (LoopB i (LoopD j f)) = 
>   norm (LoopB (tupE [i, j]) (Arr shuffleE :>>> f :>>> Arr shuffleE'))
>   where
>     shuffleE = assocE' `o` (idE `crossE` assocE')
>     shuffleE' = (idE `crossE` assocE) `o` assocE

> normalize norm (LoopD i (LoopB j f)) = 
>   norm (LoopB (tupE [i, j]) (Arr shuffleE :>>> f :>>> Arr shuffleE'))
>   where
>     shuffleE = transposeE `o` assocE'
>     shuffleE' = assocE `o` transposeE

Init

> normalize norm (Init i) = norm (LoopD i (Arr swapE))

All the other cases remain unchanged 

> normalize norm e = e 

Optimized CCNF
==============

to transform loopB to loopD:

   loopB i (arr f) ---> loopD i (arr g)

we must generate the function g from f:

   g (x, z) = let (x', (y, z')) = f (x, (y, z))
              in (x', z')

> toLoopD (LoopB i (Arr f)) = LoopD i (Arr g)
>   where
>     g = do
>       f' <- f
>       [x, y, z, x', z'] <- mapM newName ["x", "y", "z", "x'", "z'"]
>       return $ LamE [TupP [VarP x, VarP z]] $ LetE 
>                [ValD (TupP [VarP x', TupP [VarP y, VarP z']]) (NormalB $
>                 AppE f' (TupE [VarE x, TupE [VarE y, VarE z]])) []]
>                (TupE [VarE x', VarE z'])
> toLoopD x = x

Notice the recursively defined variable y, and it'll prevent GHC from inlining
g and f.  So we must further transform f into a set of let-expressions, and
move the definition of y inside. Here is a set of transformations we do to
function g:

1.  Transform function applications to let-expressions.

  (\x -> e1) e2  === let x = e2 in e1

> toLet :: Exp -> Exp
> toLet = G.everywhere (G.mkT aux)
>   where
>     aux (AppE (LamE [pat] body) arg) = LetE [ValD pat (NormalB arg) []] body
>     aux (AppE (LamE (pat:ps) body) arg) = LamE ps (LetE [ValD pat (NormalB arg) []] body)
>     aux x = x

2.  TODO: For every pattern match agains tuples, unfold right-hand-side until it is a
    tuple too, and then break up into a list of let-expressions. 

Auxiliary Functions
===================

> f `o` g = appE (appE [|\f g x -> f (g x)|] f) g
> f `crossE` g = appE (appE [|\f g x -> (f $ fst x, g $ snd x)|] f) g
> idE = [|\x -> x|]
> dupE = [|\x -> (x, x)|]
> swapE = [|\x -> (snd x, fst x)|]
> curryE = [|\f x y -> f (x, y)|]
> uncurryE = [|\f x -> f (fst x) (snd x)|]
> assocE = [|\x -> (fst $ fst x, (snd $ fst x, snd x))|]
> assocE' = [|\x -> ((fst x, fst $ snd x), snd $ snd x)|]
> juggleE = assocE' `o` (idE `crossE` swapE) `o` assocE
> transposeE = [|\x -> ((fst $ fst x, fst $ snd x),
>                       (snd $ fst x, snd $ snd x))|]


