module TCInstances where

import TCType
import AbsChapel
import Data.Maybe

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
            Array c' (Lit _ (LInt d)) -> TArr (fromInteger d :: Int) $ helper c' b
            _           -> TError


-- Literals have TCType
instance TCTypeable Literal where
    tctypeOf x = case x of
        LBool _     -> TBool
        LChar _     -> TChar
        LInt _      -> TInt
        LReal _     -> TReal
        LString _   -> TString
        LArr lits   ->
            if null lits
            then TError
            else let
                litsT = map tctypeOf lits
                t = foldl1 (supremum) litsT
                in case t of
                    TError  -> TError
                    _       -> TArr (length lits) t

-- Maybe have TCType
instance (TCTypeable a) => TCTypeable (Maybe a) where
    tctypeOf (Just x) = tctypeOf x
    tctypeOf Nothing  = TError