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
    | TArr Bool Int TCType                  -- Array Checked Dim Type
    | TFun TCType Intent [(TCType, Intent)] -- Function RetType RetIntent [(ArgType, ArgIntent)]
    deriving (Eq)


toTCT :: (Functor f) => f a -> f TCType
toTCT = fmap (const TVoid) 


-- extract a tctype from other data types
class TCTypeable a where
    tctypeOf :: a -> TCType


instance Show TCType where
    show t = case t of
        TError      -> "error_type"
        TVoid       -> "void"
        TBool       -> "bool"
        TChar       -> "char"
        TInt        -> "int"
        TReal       -> "real"
        TString     -> "string"
        TPoint t'   -> ("*"++) $ show t'
        TArr _ d t' -> (("[" ++ (show d) ++ "]")++) $ show t'
        TFun _ _ _  -> "function"


-- Poset structure of TCType
data TOrdering = TLess | TEqual | TGreater | TNotComparable
    deriving (Eq, Show)

compare' :: TCType -> TCType -> TOrdering
TBool `compare'` TInt = TLess
TBool `compare'` TReal = TLess

TChar `compare'` TInt    = TLess
TChar `compare'` TReal  = TLess

TInt `compare'` TReal   = TLess
TInt `compare'` TBool = TGreater
TInt `compare'` TChar    = TGreater

TReal `compare'` TBool = TGreater
TReal `compare'` TInt   = TGreater
TReal `compare'` TChar  = TGreater

TError `compare'` TError = TEqual
_ `compare'` TError      = TLess
TError `compare'` _      = TGreater

TArr _ d1 t1 `compare'` TArr _ d2 t2 = if d1 /= d2
    then                  TNotComparable
    else t1 `compare'` t2

TFun _ _ _ `compare'` TFun _ _ _ = TNotComparable

x `compare'` y = if x == y
    then                  TEqual
    else                  TNotComparable


-- S `subtypeOf` T means that S can be coerced to T
subtypeOf :: TCType -> TCType -> Bool
s `subtypeOf` t = let st = s `compare'` t in st == TLess || st == TEqual


-- Name from lattice-theory (perché fa figo ovviamente)
-- If types are comparable, then return the max
-- else TError
supremum :: TCType -> TCType -> TCType
supremum x y
    | x `compare'` y == TLess       = y
    | x `compare'` y == TEqual      = x
    | x `compare'` y == TGreater    = x
    | otherwise                     = TError