module TypeChecker where

import AbsGarpezpp
import Data.Maybe
import ErrTC


-- Given an RExp, returns Just the value of the RExp or Nothing in case of a non-const expression.
-- Const-expressions are those with only literals or id of constants.
-- (To be done)
constexpr :: RExp -> Maybe Int
constexpr r = Just 5

-- Type annotation for the type-checker
data TCType 
    = TVoid
    | TBool
    | TChar
    | TInt
    | TFloat
    | TString
    | TPoint TCType                     -- Pointer to
    | TArr (Int, TCType)                -- Array (Dim, Type)
    | TFun (TCType, [(TCType, PassBy)]) -- Function (Return type, [Parameter Type, Parameter Modality])
    deriving (Show, Eq)

-- Class to have a "universal" converter to TCType
class TCTypeable a where
    toTCType :: a -> TCType


-- Basic types are TCTypes
instance TCTypeable Basic where
    toTCType x = case x of
        BBool -> TBool
        BChar -> TChar
        BInt -> TInt
        BFloat -> TFloat
        BString -> TString


-- Literals of some type have TCTypes
instance TCTypeable Literal where
    toTCType x = case x of
        LBool _ -> TBool
        LChar _ -> TChar
        LInt _ -> TInt
        LFloat _ -> TFloat
        LString _ -> TString

-- General Types are TCTypes
instance TCTypeable Type where
    toTCType x = case x of
        Type b Simple -> toTCType b
        Type b c -> helper (toTCType b) c where     -- helper to pass the type "down" the declaration tree
            helper t c = case c of
                Simple -> t
                Pointer c' -> TPoint $ helper t c'
                Array c' r -> TArr (fromJust $ constexpr r, helper t c') -- Assume r a constexper (this is checked before)

-- Return types are TCTypes
instance TCTypeable RType where
    toTCType x = case x of
        RVoid -> TVoid
        RBasic b -> toTCType b
        RRef t -> toTCType t

-- Function declarations have TCType
instance TCTypeable FDecl where
    toTCType (FDecl rt _ ps _) = TFun (r, ls) where
        r = toTCType rt
        ls = map (\(Param t p _) -> (toTCType t, p)) ps



-- TODO leastGeneral
-- leastGeneral :: TCType -> TCType -> ErrTC TCType