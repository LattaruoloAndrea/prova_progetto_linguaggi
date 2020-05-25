module CompileTime where

import AbsChapel
import Data.Maybe
import Data.Char -- ord, chr
import TCType
import TCInstances

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

litRel :: (Ord a) => (a -> a -> Bool) -> LiteralComparison
litRel op = \x y -> case (x, y) of
    (LBool   a, LBool   b)  -> return $ a `op` b
    (LChar   a, LChar   b)  -> return $ a `op` b
    (LInt    a, LInt    b)  -> return $ a `op` b
    (LReal   a, LReal   b)  -> return $ a `op` b
    (LString a, LString b)  -> return $ a `op` b
    _                       -> Nothing

overload' :: LiteralComparison -> LiteralComparison
overload' op = \x y -> let t = (tctypeOf x) `supremum` (tctypeOf y) in case t of
    TBool   -> x `op` y
    TChar   -> x `op` y
    TInt    -> (toLInt x) `op` (toLInt y)
    TReal   -> (toLReal x) `op` (toLReal y)
    TString -> (toLString x) `op` (toLString y)
    _       -> Nothing


-- Comparison dispatcher
litComp :: CompOp -> LiteralComparison
litComp x = overload' . litRel $ case x of
    Lt  -> (<)
    Leq -> (<=)
    Eq  -> (==)
    Neq -> (/=)
    Geq -> (>=)
    Gt  -> (>)