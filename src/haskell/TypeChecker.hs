module TypeChecker where

import AbsChapel
import TCType
import TCInstances
import Locatable
import Env
import Control.Monad (unless, when)
import qualified ErrM as EM
import qualified ErrT as ET

-- Copied from Skel --------------------------------------
type Result = EM.Err ()

failure :: Show a => a -> Result
failure x = EM.Bad $ "Undefined case: " ++ show x
----------------------------------------------------------




-- INFER RIGHT EXPRESSIONS //////////////////////////////////////////////////////////////////////////////

inferRExp :: Env -> RExp -> EM.Err TCType
inferRExp env rexp = case rexp of
    Or _ r1 r2 -> do
        t1 <- inferRExp env r1
        t2 <- inferRExp env r2
        unless (t1 == TBool && t2 == TBool) (EM.Bad "Operands are not of type bool in a OR expression.")
        return TBool

    And _ r1 r2 -> do
        t1 <- inferRExp env r1
        t2 <- inferRExp env r2
        unless (t1 == TBool && t2 == TBool) (EM.Bad "Operands are not of type bool in a AND expression.")
        return TBool
    
    Not _ r -> do
        t <- inferRExp env r
        unless (t == TBool) (EM.Bad "Operand is not of type bool in a NOT expression.")
        return TBool
        
    Comp _ r1 _ r2 -> do
        t1 <- inferRExp env r1
        t2 <- inferRExp env r2
        let t = supremum t1 t2
        when (t == TError) (EM.Bad "Operands are not compatible in a COMPARISON expression.")
        return TBool

    Arith _ r1 _ r2 -> do
        t1 <- inferRExp env r1
        t2 <- inferRExp env r2
        let t = supremum t1 t2
        when (t == TError) (EM.Bad "Operands are not compatible in an ARITHMETIC expression.")
        return t

    Sign _ _ r -> inferRExp env r

    RefE _ l -> do
        t <- inferLExp env l
        return $ TPoint t

    RLExp _ l -> inferLExp env l
    
    ArrList _ rs -> case rs of
        [] -> EM.Bad "Error: empty array initializer."
        _  -> do
            t <- foldl1 supremumM $ map (inferRExp env) rs
            when (t == TError) (EM.Bad "Error: types in an array initializer are not compatible.")
            return $ TArr (length rs) t
    
    FCall _ ident rs -> lookType (idName ident) env -- @TODO: check right numbers and types of rs

    PredR _ pread -> return $ tctypeOf pread

    Lit _ literal -> return $ tctypeOf literal


-- INFER LEFT EXPRESSIONS //////////////////////////////////////////////////////////////////////////////

inferLExp :: Env -> LExp -> EM.Err TCType
inferLExp env lexp = case lexp of
    Deref l     -> do
        t <- inferLExp env l
        case t of
            TPoint t' -> return t'
            _         -> EM.Bad $ "Error: trying to dereference a non-pointer variable."

    Access l r  -> do
        ta <- inferLExp env l
        ti <- inferRExp env r
        when (ti `supremum` TInt /= TInt) (EM.Bad "Error: array index is not an integer in an ARRAY ACCESS.")
        case ta of
            TArr _ t -> return t
            _        -> EM.Bad $ "Error: left expression is not an array in an ARRAY ACCESS."

    Name ident  -> lookType (idName ident) env


-- CHECK VALIDITY OF STATEMENTS /////////////////////////////////////////////////////////////////////////

checkStm :: Env -> Stm -> EM.Err () -- To be changed to ET.Err ()
checkStm env stm = let x = "Dummy" in case stm of -- Dummy x to be deleted once completed all patterns
    StmBlock block -> failure x
    StmCall ident rs -> failure x
    PredW pwrite r -> failure x
    Assign l _ r -> failure x
    StmL l -> failure x
    If r stm -> failure x
    IfElse r s1 s2 -> failure x
    While r s -> failure x
    DoWhile s r -> failure x
    For ident rng s -> failure x
    
    JmpStm jmp -> checkJmp env jmp




-- checkBlock :: Env -> Block -> EM.Err ()
-- checkBlock env block = do
--     let env' = pushContext env



-- CHECK VALIDITY OF JUMP STATEMENTS ///////////////////////////////////////////////////////////////////

checkJmp :: Env -> Jump -> EM.Err () -- To be changed to ET.Err ()
checkJmp env jmp = case jmp of
    Return _    -> unless ((returns . head) env == TVoid) (EM.Bad "Error: wrong return type.")

    ReturnE _ r -> do
        t <- inferRExp env r
        let t' = (returns . head) env
        unless (t == t') (EM.Bad "Error: wrong return type.")

    Break _     -> unless ((inWhile . head) env) (EM.Bad "Error: cannot use break in a non-(do-)while statement.")
    
    Continue _  -> unless ((inWhile . head) env) (EM.Bad "Error: cannot use continue in a non-(do-)while statement.")