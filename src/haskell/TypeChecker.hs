module TypeChecker where

import AbstractSyntaxTree
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
    deriving (Eq)

instance Show TCType where
    show (TVoid) = "void"
    show (TBool) = "bool"
    show (TString) = "string"
    show (TInt) = "int"
    show (TFloat) = "float"
    show (TChar) = "char"
    show (TPoint t) = "pointer"
    show (TArr (i, t)) = "array"

-- Class to have a "universal" converter to TCType
class TCTyped a where
    tctOf :: a -> TCType


-- Basic types are TCTypes
instance TCTyped Basic where
    tctOf x = case x of
        BBool -> TBool
        BChar -> TChar
        BInt -> TInt
        BFloat -> TFloat
        BString -> TString


-- Literals of some type have TCTypes
instance TCTyped Literal where
    tctOf x = case x of
        LBool _ -> TBool
        LChar _ -> TChar
        LInt _ -> TInt
        LFloat _ -> TFloat
        LString _ -> TString

-- General Types are TCTypes
instance TCTyped Type where
    tctOf x = case x of
        Type b Simple -> tctOf b
        Type b c -> helper (tctOf b) c where     -- helper to pass the type "down" the declaration tree
            helper t c = case c of
                Simple -> t
                Pointer c' -> TPoint $ helper t c'
                Array c' r -> TArr (fromJust $ constexpr r, helper t c') -- Assume r a constexper (this is checked before)

-- Return types are TCTypes
instance TCTyped RType where
    tctOf x = case x of
        RVoid -> TVoid
        RBasic b -> tctOf b
        RRef t -> tctOf t

-- Function declarations have TCType
instance TCTyped FDecl where
    tctOf (FDecl rt _ ps _) = TFun (r, ls) where
        r = tctOf rt
        param2pair = (\(Param t p _) -> (tctOf t, p)
        ls = map (param2pair . value) ps -- ps is a [Posn Param]



-- qui EM.Err per import qualified ErrM as EM

mostGeneral :: TCType -> TCType -> EM.Err TCType
mostGeneral t1 t2 = case (t1,t2) of
    (TBool, TBool) -> EM.Ok TBool
    (TString, TString) -> EM.Ok TString
    (TChar, TChar) -> EM.Ok TChar
    (TChar, TInt) -> EM.Ok TInt
    (TChar, TFloat) -> EM.Ok TFloat
    (TInt, TChar) -> EM.Ok TInt
    (TInt, TInt) -> EM.Ok TInt
    (TInt, TFloat) -> EM.Ok TFloat
    (TFloat, TChar) -> EM.Ok TFloat
    (TFloat, TInt) -> EM.Ok TFloat
    (TFloat, TFloat) -> EM.Ok TFloat
    (TPoint t1', TPoint t2') -> mostGeneral t1' t2'
    (TArr (i1, t1'), TArr (i2, t2')) -> if (i1 == i2) then
            mostGeneral t1' t2'
        else
            EM.Bad "arrays have different lengths"
    (_,_) -> EM.Bad ("Types " ++ (show t1) ++ " and " ++ (show t2) ++ " are not compatible")
