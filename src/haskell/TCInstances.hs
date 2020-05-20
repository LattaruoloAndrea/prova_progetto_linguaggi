module TCInstances where

import TCType
import AbsChapel
import Env

-- Basic type have TCType
instance TCTypeable Basic where
    tctypeOf x = case x of
        BVoid   -> TVoid
        BBool   -> TBool
        BChar   -> TChar
        BInt    -> TInt
        BReal   -> TReal
        BString -> TString

-- Type have TCType
-- it is required that an Array has already an integer literal for size
instance TCTypeable Type where
    tctypeOf (Type c b) = helper c b where
        helper c b = case c of
            Simple      -> tctypeOf b
            Pointer c'  -> TPoint $ helper c' b
            Array c' (Lit (LInt d)) -> TArr (fromInteger d :: Int) $ helper c' b
            _           -> TError


-- Entry have TCType
instance TCTypeable Entry where
    tctypeOf x = case x of
        Var _ t     -> t
        Const _ t _ -> t
        Fun _ t     -> t