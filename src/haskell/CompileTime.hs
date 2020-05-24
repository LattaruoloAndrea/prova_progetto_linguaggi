module CompileTime where

import AbsChapel (Literal(..))
import Data.Maybe
import Data.Char -- ord, chr

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