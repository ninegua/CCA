{-# LANGUAGE MagicHash, CPP, TemplateHaskell, FlexibleInstances, TypeSynonymInstances #-}

module Language.Haskell.TH.Instances where
import Data.Ratio
import GHC.Exts
import GHC.Prim
import Language.Haskell.TH.Syntax

instance Lift a => Lift (Q a) where
  lift x = x >>= \x -> [| return x |]

instance Lift Exp where
  lift (VarE name) = [|VarE name|]
  lift (ConE name) = [|ConE name|]
  lift (LitE lit)  = [|LitE lit|]
  lift (AppE e1 e2) = [|AppE e1 e2|]
  lift (InfixE e1 e2 e3) = [|InfixE e1 e2 e3|]
  lift (LamE p e) = [|LamE p e|]
  lift (TupE e) = [|TupE e|]
  lift (CondE e1 e2 e3) = [|CondE e1 e2 e3|]
  lift (LetE d e) = [|LetE d e|]
  lift (CaseE e m) = [|CaseE e m|]
  lift (DoE s) = [|DoE s|]
  lift (CompE s) = [|CompE s|]
  lift (ArithSeqE r) = [|ArithSeqE r|]
  lift (ListE e) = [|ListE e|]
  lift (SigE e t) = [|SigE e t|]
  lift (RecConE n f) = [|RecConE n f|]
  lift (RecUpdE e f) = [|RecUpdE e f|]

instance Lift Name where
  lift (Name o f) = [|Name o f|]

instance Lift OccName where
  lift s = let s' = occString s in [|mkOccName s'|]

fromInt (I# i) = i

#if __GLASGOW_HASKELL__ >= 612

instance Lift ModName where
  lift = lift . modString

instance Lift PkgName where
  lift = lift . pkgString

#if ! MIN_VERSION_template_haskell(2,10,0)

instance Lift Pred where
  lift (ClassP n ts) = [|ClassP n ts|]
  lift (EqualP t t2) = [|EqualP t t2|]

#endif

#if MIN_VERSION_template_haskell(2,8,0)

instance Lift Kind where
  lift StarT = [|StarT|]
  lift (ForallT n c t) = [|ForallT n c t|]
  lift (VarT n) = [|VarT n|]
  lift (ConT n) = [|ConT n|]
  lift (TupleT i) = [|TupleT i|]
  lift (ArrowT) = [|ArrowT|]
  lift (ListT) = [|ListT|]
  lift (AppT t1 t2) = [|AppT t1 t2|]

#else

instance Lift Kind where
  lift StarK = [|StarK|]
  lift (ArrowK k1 k2) = [|ArrowK k1 k2|]

#endif

instance Lift TyVarBndr where
  lift (PlainTV n) = [|PlainTV n|]
  lift (KindedTV n k) = [|KindedTV n k|]

#endif

#if MIN_VERSION_template_haskell(2,10,0)

instance Lift NameFlavour where
  lift NameS = [|NameS|]
  lift (NameQ n) = [|NameQ n|]
  lift (NameU i) = [|NameU (fromInt i)|]
  lift (NameL i) = [|NameL (fromInt i)|]
  lift (NameG n p m) = [|NameG n p m|]

#else

instance Lift NameFlavour where
  lift NameS = [|NameS|]
  lift (NameQ n) = [|NameQ n|]
  lift (NameU i) = let i' = I# i in [|NameU (fromInt i')|]
  lift (NameL i) = let i' = I# i in [|NameL (fromInt i')|]
  lift (NameG n p m) = [|NameG n p m|]

#endif

instance Lift Range where
  lift (FromR e) = [|FromR e|]
  lift (FromThenR e1 e2) = [|FromThenR e1 e2|]
  lift (FromToR e1 e2) = [|FromToR e1 e2|]
  lift (FromThenToR e1 e2 e3) = [|FromThenToR e1 e2 e3|]

instance Lift Stmt where
  lift (BindS p e) = [|BindS p e|]
  lift (LetS d) = [|LetS d|]
  lift (NoBindS e) = [|NoBindS e|]
  lift (ParS s) = [|ParS s|]

instance Lift Match where
  lift (Match p b d) = [|Match p b d|]


#if ! MIN_VERSION_template_haskell(2,8,0)

instance Lift Type where
  lift (ForallT n c t) = [|ForallT n c t|]
  lift (VarT n) = [|VarT n|]
  lift (ConT n) = [|ConT n|]
  lift (TupleT i) = [|TupleT i|]
  lift (ArrowT) = [|ArrowT|]
  lift (ListT) = [|ListT|]
  lift (AppT t1 t2) = [|AppT t1 t2|]

#endif

instance Lift Pat where
  lift (LitP l) = [|LitP l|]
  lift (VarP n) = [|VarP n|]
  lift (TupP p) = [|TupP p|]
  lift (ConP n p) = [|ConP n p|]
  lift (InfixP p1 n p2) = [|InfixP p1 n p2|]
  lift (TildeP p) = [|TildeP p|]
  lift (AsP n p) = [|AsP n p|]
  lift (WildP) = [|WildP|]
  lift (RecP n f) = [|RecP n f|]
  lift (ListP p) = [|ListP p|]
  lift (SigP p t) = [|SigP p t|]

instance Lift Lit where
  lift (CharL c) = [|CharL c|]
  lift (StringL s) = [|StringL s|]
  lift (IntegerL i) = [|IntegerL i|]
  lift (RationalL r) = [|RationalL r|]
  lift (IntPrimL i) = [|IntPrimL i|]
#if __GLASGOW_HASKELL__ >= 610
  lift (WordPrimL i) = [|WordPrimL i|]
#endif
  lift (FloatPrimL r) = [|FloatPrimL r|]
  lift (DoublePrimL r) = [|DoublePrimL r|]

instance Lift Dec where
  lift (FunD n c) = [|FunD n c|]
  lift (ValD p b d) = [|ValD p b d|]
  lift (DataD c n l1 l2 l3) = [|DataD c n l1 l2 l3|]
  lift (NewtypeD c n l1 c' l2) = [|NewtypeD c n l1 c' l2|]
  lift (TySynD n l t) = [|TySynD n l t|]
  lift (ClassD c n l1 l2 l3) = [|ClassD c n l1 l2 l3|]
  lift (InstanceD c t d) = [|InstanceD c t d|]
  lift (SigD n t) = [|SigD n t|]
  lift (ForeignD f) = [|ForeignD f|]

instance Lift NameSpace where
  lift (VarName) = [|VarName|]
  lift (DataName) = [|DataName|]
  lift (TcClsName) = [|TcClsName|]

instance Lift Body where
  lift (GuardedB l) = [|GuardedB l|]
  lift (NormalB e) = [|NormalB e|]

instance Lift Clause where
  lift (Clause p b d) = [|Clause p b d|]

instance Lift Con where
  lift (NormalC n m) = [|NormalC n m|]
  lift (RecC n s) = [|RecC n s|]
  lift (InfixC s1 n s2) = [|InfixC s1 n s2|]
  lift (ForallC n c c') = [|ForallC n c c'|]

instance Lift FunDep where
  lift (FunDep l1 l) = [|FunDep l1 l|]

instance Lift Foreign where
  lift (ImportF c f s n t) = [|ImportF c f s n t|]
  lift (ExportF c s n t) = [|ExportF c s n t|]

instance Lift Guard where
  lift (NormalG e) = [|NormalG e|]
  lift (PatG s) = [|PatG s|]

instance Lift Strict where
  lift (IsStrict) = [|IsStrict|]
  lift (NotStrict) = [|NotStrict|]

instance Lift Safety where
  lift (Unsafe) = [|Unsafe|]
  lift (safe) = [|safe|]

instance Lift Callconv where
  lift (CCall) = [|CCall|]
  lift (StdCall) = [|StdCall|]

#if ! MIN_VERSION_template_haskell(2,10,0)

instance Lift Rational where
  lift r = let n = numerator r
               d = denominator r
           in [|n % d|]

instance Lift Double where
  lift d = [| D# $(return (LitE (DoublePrimL (toRational d)))) |]

#endif
