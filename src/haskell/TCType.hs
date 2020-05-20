module TCType where

import AbsChapel (Intent)

-- Type system
data TCType 
    = TError
    | TVoid
    | TBool
    | TChar
    | TInt
    | TReal
    | TString
    | TPoint TCType                         -- Pointer to
    | TArr Int TCType                       -- Array Dim Type
    | TFun TCType Intent [(TCType, Intent)] -- Function RetType RetIntent [(ArgType, ArgIntent)]
    deriving (Eq, Show)


-- extract a tctype from other data types
class TCTypeable a where
    tctypeOf :: a -> TCType


-- Poset structure of TCType
data TOrdering = TLess | TEqual | TGreater | TNotComparable
    deriving (Eq, Show)

compare' :: TCType -> TCType -> TOrdering
TChar `compare'` TInt    = TLess
TChar `compare'` TReal  = TLess
TChar `compare'` TString = TLess

TInt `compare'` TReal   = TLess
TInt `compare'` TChar    = TGreater

TReal `compare'` TInt   = TGreater
TReal `compare'` TChar  = TGreater

TError `compare'` TError = TEqual
_ `compare'` TError      = TLess
TError `compare'` _      = TGreater

TArr d1 t1 `compare'` TArr d2 t2 = if d1 /= d2
    then                  TNotComparable
    else t1 `compare'` t2

TFun _ _ _ `compare'` TFun _ _ _ = TNotComparable

x `compare'` y = if x == y
    then                  TEqual
    else                  TNotComparable


-- Name from lattice-theory (perché fa figo ovviamente)
supremum :: TCType -> TCType -> TCType
supremum x y
    | x `compare'` y == TLess       = y
    | x `compare'` y == TEqual      = x
    | x `compare'` y == TGreater    = x
    | otherwise                     = TError