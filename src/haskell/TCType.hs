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
    | TPoint TCType                     	-- Pointer to
    | TArr Int TCType                		-- Array Dim Type
    | TFun TCType Intent [TCType, Intent] 	-- Function RetType RetIntent [ArgType, ArgIntent]
	deriving (Eq)


-- extract a tctype from other data types
class TCTypeable a where
    tctypeOf :: a -> TCType


-- Poset structure of TCType
data TOrdering = TLess | TEqual | TGreater | TNotComparable

compare' :: TCType -> TCType -> TOrdering
TChar `compare'` TInt    = TLess
TChar `compare'` TFloat  = TLess
TChar `compare'` TString = TLess

TInt `compare'` TFloat   = TLess
TInt `compare'` TChar    = TGreater

TFloat `compare'` TInt   = TGreater
TFloat `compare'` TChar  = TGreater

TError `compare'` TError = TEqual
_ `compare'` TError      = TLess
TError `compare'` _      = TGreater

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