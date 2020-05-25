module CompileTime where

import AbsChapel
import Data.Maybe
import Data.Char -- ord, chr
import TCType
import TCInstances
import Control.Monad (guard)


-- Literal coercions /////////////////////////////////////////////////////////////

toLInt :: Literal -> Literal
toLInt (LChar a)    = LInt . toInteger . ord $ a
toLInt i@(LInt _)   = i
-- undefined patterns

toLReal :: Literal -> Literal
toLReal c@(LChar a) = toLReal . toLInt $ c
toLReal (LInt a)    = LReal . fromInteger $ a
toLReal r@(LReal _) = r
-- undefined patterns

toLString :: Literal -> Literal
toLString (LChar a)     = LString [a]
toLString s@(LString _) = s
-- undefined patterns

-- Binary operation overloading abstractions ///////////////////////////////////////
type LStringOperator = Literal -> Literal -> Maybe Literal
type LIntOperator    = Literal -> Literal -> Maybe Literal
type LRealOperator   = Literal -> Literal -> Maybe Literal
type LiteralOperator = Literal -> Literal -> Maybe Literal

-- 1. Abstraction on the "pure" operations
strBinary :: (String -> String -> String) -> LStringOperator
strBinary op = \x y -> case (x,y) of
    (LString a, LString b) -> Just . LString $ a `op` b
    _                      -> Nothing

intBinary :: (Integer -> Integer -> Integer) -> LIntOperator
intBinary op = \x y -> case (x,y) of
    (LInt a, LInt b) -> Just . LInt $ a `op` b
    _                -> Nothing

realBinary :: (Double -> Double -> Double) -> LRealOperator
realBinary op = \x y -> case (x,y) of
    (LReal a, LReal b) -> Just . LReal $ a `op` b
    _                -> Nothing


-- 2. Abstraction on the overloading check

-- overload abstracts the process of overloading given an int and a real version of an operator
overload :: LIntOperator -> LRealOperator -> LiteralOperator
overload intOp realOp = \x y -> case (x, y) of
    (LChar _, _)        -> overload intOp realOp (toLInt x) y
    (LInt _, LChar _)   -> x `intOp` (toLInt y)
    (LInt _, LInt _)    -> x `intOp` y
    (LInt _, LReal _)   -> overload intOp realOp (toLReal x) y
    (LReal _, LChar _)  -> x `realOp` (toLReal y)
    (LReal _, LInt _)   -> x `realOp` (toLReal y)
    (LReal _, LReal _)  -> x `realOp` y
    _                   -> Nothing


-- Concrete addition ////////////////////////////////////////////////////////////////

strAdd :: LStringOperator
strAdd = strBinary (++)

intAdd :: LIntOperator
intAdd = intBinary (+)

realAdd :: LRealOperator
realAdd = realBinary (+)


litAdd :: LiteralOperator
x `litAdd` y = case (x, y) of
    (LChar _, LString _)    -> (toLString x) `strAdd` y
    (LString _, _)          -> x `strAdd` y
    _                       -> overload intAdd realAdd x y



-- Concrete subtraction ////////////////////////////////////////////////////////////////

intSub :: LIntOperator
intSub = intBinary (-)

realSub :: LRealOperator
realSub = realBinary (-)

litSub :: LiteralOperator
litSub = overload intSub realSub

-- Concrete multiplication ////////////////////////////////////////////////////////////

intMul :: LIntOperator
intMul = intBinary (*)

realMul :: LRealOperator
realMul = realBinary (*)

litMul :: LiteralOperator
litMul = overload intMul realMul

-- Concrete division //////////////////////////////////////////////////////////////////

intDiv :: LIntOperator
intDiv = intBinary div

realDiv :: LRealOperator
realDiv = realBinary (/)

litDiv :: LiteralOperator
litDiv = overload intDiv realDiv

-- Concrete exponentiation ///////////////////////////////////////////////////////////

litPow :: LiteralOperator
x `litPow` c@(LChar _) = x `litPow` (toLInt c)

x `litPow` y@(LInt e) = case x of
    LChar _     -> (toLInt x) `litPow` y
    LInt a      -> Just . LInt $ a ^ e
    LReal a     -> Just . LReal $ a ^ e
    _           -> Nothing

_ `litPow` _ = Nothing -- undefined for non integer exponent

-- Concrete remainder ///////////////////////////////////////////////////////////////

litMod :: LiteralOperator
c@(LChar _) `litMod` x = (toLInt c) `litMod` x

i@(LInt a) `litMod` x = case x of
    LChar _     -> i `litMod` (toLInt x)
    LInt  n     -> Just . LInt $ a `mod` n
    _           -> Nothing

_ `litMod` _ = Nothing -- undefined for non integer operands


-- Arithmetic dispatcher

litArith :: ArithOp -> LiteralOperator
litArith x = case x of
    Add -> litAdd
    Sub -> litSub
    Mul -> litMul
    Div -> litDiv
    Mod -> litMod
    Pow -> litPow



-- COMPARISON OPERATORS ////////////////////////////////////////////////////////////////

type LiteralComparison = Literal -> Literal -> Maybe Bool

data LitOrd = LitLT | LitEQ | LitGT | LitNC
    deriving (Eq, Show)

toLitOrd :: Ordering -> LitOrd
toLitOrd x = case x of
    LT  -> LitLT
    EQ  -> LitEQ
    GT  -> LitGT

-- Strict comparison
strictCompare :: Literal -> Literal -> LitOrd
strictCompare x y = case (x, y) of
    (LBool   a, LBool b)    -> toLitOrd $ a `compare` b
    (LChar   a, LChar b)    -> toLitOrd $ a `compare` b
    (LInt    a, LInt  b)    -> toLitOrd $ a `compare` b
    (LReal   a, LReal b)    -> toLitOrd $ a `compare` b
    (LString a, LString b)  -> toLitOrd $ a `compare` b
    _                       -> LitNC


-- Comparison with type compatibilities
overloadCompare :: Literal -> Literal -> LitOrd
overloadCompare x y = case (tctypeOf x) `supremum` (tctypeOf y) of
    TBool   -> x `strictCompare` y
    TChar   -> x `strictCompare` y
    TInt    -> (toLInt x)    `strictCompare` (toLInt y)
    TReal   -> (toLReal x)   `strictCompare` (toLReal y)
    TString -> (toLString x) `strictCompare` (toLString y)
    _       -> LitNC


-- Comparison operator dispatcher
litComp :: CompOp -> LiteralComparison
litComp op = \x y -> let rel = x `overloadCompare` y in case rel of
    LitNC -> Nothing
    _     -> Just $ case op of
        Lt  -> rel == LitLT
        Leq -> rel == LitLT || rel == LitEQ
        Eq  -> rel == LitEQ
        Neq -> rel /= LitEQ
        Geq -> rel /= LitLT
        Gt  -> rel == LitGT



-- ARRAY ACCESS ///////////////////////////////////////////////////////////////////////

-- Access element at position i in an array of Literals
-- (Precondition) i must have tctype <= TInt
litAccess :: Literal -> Literal -> Maybe Literal
litAccess arr@(LArr lits) i = do
    let (LInt i') = toLInt i
    guard (i' < (toInteger . length $ lits))
    guard (0 <= i')
    return $ lits !! (fromInteger i')