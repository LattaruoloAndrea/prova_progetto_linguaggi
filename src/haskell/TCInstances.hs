{-# LANGUAGE FlexibleInstances #-}

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
instance TCTypeable (Type t) where
    tctypeOf (Type c b) = helper c b where
        helper c b = case c of
            Simple      -> tctypeOf b
            Pointer c'  -> TPoint $ helper c' b
            Array ch c' (Lit _ (LInt d) t)  -> TArr ch (fromInteger d :: Int) $ helper c' b
            _           -> TError


-- Literals have TCType
instance TCTypeable Literal where
    tctypeOf x = case x of
        LBool _     -> TBool
        LChar _     -> TChar
        LInt _      -> TInt
        LReal _     -> TReal
        LString _   -> TString
        LArr c lits ->
            if null lits
            then TError
            else let
                litsT = map tctypeOf lits
                t = foldl1 (supremum) litsT
                in case t of
                    TError  -> TError
                    _       -> TArr c (length lits) t

-- Maybe have TCType
instance (TCTypeable a) => TCTypeable (Maybe a) where
    tctypeOf (Just x) = tctypeOf x
    tctypeOf Nothing  = TError


-- (RExp TCType) have TCType
instance TCTypeable (RExp TCType) where
    tctypeOf = reTy

-- (LExp TCType) have TCType
instance TCTypeable (LExp TCType) where
    tctypeOf x = case x of
        Deref  _ t   -> t
        Access _ _ t -> t
        Name   _ t   -> t


-- (CompOp TCType) have TCType
instance TCTypeable (CompOp TCType) where
    tctypeOf x = case x of
        Lt  t -> t
        Leq t -> t
        Eq  t -> t
        Neq t -> t
        Geq t -> t
        Gt  t -> t

-- (ArithOp TCType) have TCType
instance TCTypeable (ArithOp TCType) where
    tctypeOf x = case x of
        Add t -> t
        Sub t -> t
        Mul t -> t
        Div t -> t
        Mod   -> TInt
        Pow t -> t