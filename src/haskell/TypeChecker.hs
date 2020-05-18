{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module TypeChecker where

import AbstractSyntaxTree
import Data.Maybe
import ErrTC
import Control.Lens

import Control.Monad (unless)

import qualified Data.Map.Lazy as M


-- Given an RExp, returns Just the value of the RExp or Nothing in case of a non-const expression.
-- Const-expressions are those with only literals or id of constants.
-- (To be done)
constexpr :: RExp -> Maybe Int
constexpr r = Just 5

-- Type annotation for the type-checker
data TCType 
    = TError
    | TVoid
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
    tctOf (FDecl rt _ ps _ _) = TFun (r, ls) where
        r = tctOf rt
        param2pair = (\(Param t p _ _) -> (tctOf t, p)
        ls = map (param2pair . value) ps -- ps is a [Posn Param]




mostGeneral :: TCType -> TCType -> Err TCType
mostGeneral t1 t2 = case (t1,t2) of
    (TBool, TBool) -> return TBool
    (TString, TString) -> return TString
    (TChar, TChar) -> return TChar
    (TChar, TInt) -> return TInt
    (TChar, TFloat) -> return TFloat
    (TInt, TChar) -> return TInt
    (TInt, TInt) -> return TInt
    (TInt, TFloat) -> return TFloat
    (TFloat, TChar) -> return TFloat
    (TFloat, TInt) -> return TFloat
    (TFloat, TFloat) -> return TFloat
    (TError, _) -> return TError
    (_, TError) -> return TError
    (TPoint t1', TPoint t2') -> mostGeneral t1' t2'
    (TArr (i1, t1'), TArr (i2, t2')) ->
        if (i1 == i2) then mostGeneral t1' t2'
        else bad "Error: arrays have different lengths" TError
    (_,_) -> bad ("Error: non comparable types " ++ (show t1) ++ " and " ++ (show t2)) TError









-- ENVIRONMENT
type Env = [Context]
type Name = String

data Context = Context {
      nameEntry :: (M.Map Name EnvEntry)
    , returning :: TCType
    , jumpable :: Bool
}

data EnvEntry
    = Var { _envEntryLoc :: Loc, _envEntryTctype :: TCType }
    | Const { _envEntryLoc :: Loc, _envEntryTctype :: TCType }
    | Fun { _envEntryLoc :: Loc, _envEntryTctype :: TCType }
makeFields ''EnvEntry








-- 

checkRExp :: RExp -> TCType -> Env -> Err TCType
checkRExp r t env = do
    t' <- inferRExp r env
    if 

inferLogical :: RExp -> RExp -> Env -> Err TCType
inferLogical r1 r2 env = do
    t1 <- inferRExp r1 env
    t2 <- inferRExp r2 env
    tt <- mostGeneral t1 t2
    when (tt == TError) (bad "Error: operands are not of type bool" TError)

inferComparison :: RExp -> RExp -> Env -> Err TCType
inferComparison r1 r2 env = do
    t1 <- inferRExp r1 env
    t2 <- inferRExp r2 env
    tt <- mostGeneral t1 t2
    unless (tt == TError) return TBool

inferArithmetic :: RExp -> RExp -> Env -> Err TCType
inferArithmetic r1 r2 env = do
    t1 <- inferRExp r1 env
    t2 <- inferRExp r2 env
    tt <- mostGeneral t1 t2
    when (tt == TError)

-- infer for RExp
inferRExp :: RExp -> Env -> ErrTC TCType
inferRExp r env = case r of
    Or r1 r2 _  -> inferLogical r1 r2 env
    And r1 r2 _ -> inferLogical r1 r2 env
    Not r1 _    -> inferLogical r1 LTrue env
    Lt r1 r2 _  -> inferComparison r1 r2 env
    Le r1 r2 _  -> inferComparison r1 r2 env
    Eq r1 r2 _  -> inferComparison r1 r2 env
    Neq r1 r2 _ -> inferComparison r1 r2 env
    Ge r1 r2 _  -> inferComparison r1 r2 env
    Gt r1 r2 _  -> inferComparison r1 r2 env
    Add r1 r2 _ -> inferArithmetic r1 r2 env