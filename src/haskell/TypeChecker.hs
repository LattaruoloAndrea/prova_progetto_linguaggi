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
        unless (t1 == TBool) (EM.Bad ("the operand " ++ (show r1) ++ " in an OR expression should have type bool, instead has type " ++ (show t1)))
        unless (t2 == TBool) (EM.Bad ("the operand " ++ (show r2) ++ " in an OR expression should have type bool, instead has type " ++ (show t2)))
        return TBool

    And _ r1 r2 -> do
        t1 <- inferRExp env r1
        t2 <- inferRExp env r2
        unless (t1 == TBool) (EM.Bad ("the operand " ++ (show r1) ++ " in an AND expression should have type bool, instead has type " ++ (show t1)))
        unless (t2 == TBool) (EM.Bad ("the operand " ++ (show r2) ++ " in an AND expression should have type bool, instead has type " ++ (show t2)))
        return TBool
    
    Not _ r -> do
        t <- inferRExp env r
        unless (t == TBool) (EM.Bad ("the operand " ++ (show r) ++ " in a NOT expression should have type bool, instead has type " ++ (show t)))
        return TBool
        
    Comp _ r1 _ r2 -> do
        t1 <- inferRExp env r1
        t2 <- inferRExp env r2
        let t = supremum t1 t2
        when (t == TError) (EM.Bad ("operands " ++ (show r1) ++ " (type: " ++ (show t1) ++ ") and " ++ (show r2) ++ " (type: " ++ (show t2) ++ ") are not compatible in a COMPARISON expression"))
        return TBool

    Arith _ r1 _ r2 -> do
        t1 <- inferRExp env r1
        t2 <- inferRExp env r2
        let t = supremum t1 t2
        when (t == TError) (EM.Bad ("operands " ++ (show r1) ++ " (type: " ++ (show t1) ++ ") and " ++ (show r2) ++ " (type: " ++ (show t2) ++ ") are not compatible in an ARITHMETIC expression"))
        return t

    Sign _ _ r -> inferRExp env r

    RefE _ l -> do
        t <- inferLExp env l
        return $ TPoint t

    RLExp _ l -> inferLExp env l
    
    ArrList _ rs -> case rs of
        [] -> EM.Bad "empty array initializer"
        _  -> do
            t <- foldl1 supremumM $ map (inferRExp env) rs
            when (t == TError) (EM.Bad ("types in the array initializer " ++ (show rs) ++ " are not compatible"))
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
            _         -> EM.Bad $ ("trying to dereference the non-pointer variable " ++ (show l))

    Access l r  -> do
        ta <- inferLExp env l
        ti <- inferRExp env r
        when (ti `supremum` TInt /= TInt) (EM.Bad ("array index " ++ (show r) ++ " should have tipe integer in an ARRAY ACCESS")
        case ta of
            TArr _ t -> return t
            _        -> EM.Bad $ "left expression " ++ (show l) ++ " is not an array in an ARRAY ACCESS"

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
    Return _    -> unless ((returns . head) env == TVoid) (EM.Bad "cannot return a type different from void in a PROCEDURE")

    ReturnE _ r -> do
        t <- inferRExp env r
        let t' = (returns . head) env
        unless (t == t') (EM.Bad ("the return type should be " ++ (show t'))

    Break _     -> unless ((inWhile . head) env) (EM.Bad "cannot use break in a non-(do-)while statement")
    
    Continue _  -> unless ((inWhile . head) env) (EM.Bad "cannot use continue in a non-(do-)while statement")
