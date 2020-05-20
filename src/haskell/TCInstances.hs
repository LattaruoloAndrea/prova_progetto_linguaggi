module TCInstances where

import TCType
import AbsChapel

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
    tctypeOf (Type c b) = helper c b
    where
        helper c = case c of
            Simple      -> tctypeOf
            Pointer c'  -> TPoint $ helper c'
            Array c' (Lit (LInt d)) -> TArr d $ helper c'
            _           -> TError