module TypeChecker where

import AbsChapel
import TCType
import TCInstances
import Locatable
import Env
import Control.Monad (unless, when, foldM)
import qualified ErrM as EM
import qualified ErrT as ET

-- Copied from Skel --------------------------------------
type Result a = EM.Err a

failure :: Show a => a -> Result b
failure x = EM.Bad $ "Undefined case: " ++ show x
----------------------------------------------------------




-- INFER RIGHT EXPRESSIONS //////////////////////////////////////////////////////////////////////////////

inferRExp :: Env -> RExp -> EM.Err TCType
inferRExp env rexp = case rexp of
    Or _ r1 r2 -> do
        t1 <- inferRExp env r1
        t2 <- inferRExp env r2
        unless (t1 == TBool) (EM.Bad ("Error at line " ++ (show (linOf r1)) ++ ": the operand " ++ (show r1) ++ " in an OR expression should have type bool, instead has type " ++ (show t1) ++ ", in the expression " ++ (show rexp)))
        unless (t2 == TBool) (EM.Bad ("Error at line " ++ (show (linOf r2)) ++ ": the operand " ++ (show r2) ++ " in an OR expression should have type bool, instead has type " ++ (show t2) ++ ", in the expression " ++ (show rexp)))
        return TBool

    And _ r1 r2 -> do
        t1 <- inferRExp env r1
        t2 <- inferRExp env r2
        unless (t1 == TBool) (EM.Bad ("Error at line " ++ (show (linOf r1)) ++ ": the operand " ++ (show r1) ++ " in an AND expression should have type bool, instead has type " ++ (show t1) ++ ", in the expression " ++ (show rexp)))
        unless (t2 == TBool) (EM.Bad ("Error at line " ++ (show (linOf r2)) ++ ": the operand " ++ (show r2) ++ " in an AND expression should have type bool, instead has type " ++ (show t2) ++ ", in the expression " ++ (show rexp)))
        return TBool
    
    Not _ r -> do
        t <- inferRExp env r
        unless (t == TBool) (EM.Bad ("Error at line " ++ (show (linOf r)) ++ ": the operand " ++ (show r) ++ " in a NOT expression should have type bool, instead has type " ++ (show t) ++ ", in the expression " ++ (show rexp)))
        return TBool
        
    Comp _ r1 _ r2 -> do
        t1 <- inferRExp env r1
        t2 <- inferRExp env r2
        let t = supremum t1 t2
        when (t == TError) (EM.Bad ("Error at line " ++ (show (linOf r)) ++ "operands " ++ (show r1) ++ " (type: " ++ (show t1) ++ ") and " ++ (show r2) ++ " (type: " ++ (show t2) ++ ") are not compatible in a COMPARISON expression, in the expression " ++ (show rexp)))
        return TBool

    Arith _ r1 _ r2 -> do
        t1 <- inferRExp env r1
        t2 <- inferRExp env r2
        let t = supremum t1 t2
        when (t == TError) (EM.Bad ("Error at line " ++ (show (linOf r)) ++ "operands " ++ (show r1) ++ " (type: " ++ (show t1) ++ ") and " ++ (show r2) ++ " (type: " ++ (show t2) ++ ") are not compatible in an ARITHMETIC expression, in the expression " ++ (show rexp)))
        return t

    Sign _ _ r -> inferRExp env r

    RefE _ l -> do
        t <- inferLExp env l
        case t of
            Bad s -> EM.Bad ("Error at line " ++ (show (linOf l)) ++ s ++ ", in the expression " ++ (show rexp))
            Ok t -> return $ TPoint t

    RLExp _ l -> do
        t <- inferLExp env l
        case t of
            Bad s -> EM.Bad ("Error at line " ++ (show (linOf l)) ++ s ++ ", in the expression " ++ (show rexp))
            Ok t -> return t
    
    ArrList _ rs -> case rs of
        [] -> EM.Bad ("Error at line " ++ (show (linOf rs)) ++ ": empty array initializer, in the expression " ++ (show rexp))
        _  -> do
            t <- foldl1 supremumM $ map (inferRExp env) rs
            when (t == TError) (EM.Bad ("Error at line " ++ (show (linOf rs)) ++ ": types in the array initializer " ++ (show rs) ++ " are not compatible, in the expression " ++ (show rexp)))
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
            _         -> EM.Bad $ (": trying to dereference the non-pointer variable " ++ (show l))

    Access l r  -> do
        ta <- inferLExp env l
        ti <- inferRExp env r
        when (ti `supremum` TInt /= TInt) (EM.Bad (": array index " ++ (show r) ++ " should have tipe integer in an ARRAY ACCESS"))
        case ta of
            TArr _ t -> return t
            _        -> EM.Bad $ ": left expression " ++ (show l) ++ " is not an array in an ARRAY ACCESS"

    Name ident  -> lookType (idName ident) env


-- CHECK VALIDITY OF STATEMENTS /////////////////////////////////////////////////////////////////////////

checkStm :: Env -> Stm -> EM.Err Env -- To be changed to ET.Err ()
checkStm env stm = let x = "Dummy" in case stm of -- Dummy x to be deleted once completed all patterns
    StmBlock block -> checkBlock env block

    StmCall ident rs -> do
        f <- lookFun (idName ident) env -- @TODO: check that arguments are ok with function definition
        return env

    PredW pwrite r -> do
        let t = tctypeOf pwrite
        t' <- inferRExp env r    
        unless (t' `subtypeOf` t) (EM.Bad $ "Error: type mismatch in a FUNCTION CALL.")
        return env
    
    Assign l _ r -> do -- @TODO: check that l is non-constant
        tl <- inferLExp env l
        tr <- inferRExp env r
        unless (tr `subtypeOf` tl) (EM.Bad $ "Error: type mismatch in an ASSIGNMENT.")
        return env

    StmL l -> do
        inferLExp env l
        return env

    If r stm -> do
        tr <- inferRExp env r
        unless (tr == TBool) (EM.Bad $ "Error: expression is not of type bool in the guard of an IF STATEMENT.")
        let thenEnv = pushContext env                   -- Entering the scope of true
        thenEnv' <- checkStm thenEnv stm
        return $ popContext thenEnv'                    -- Exiting the scope of true and return the new env

    IfElse r s1 s2 -> do
        tr <- inferRExp env r
        unless (tr == TBool) (EM.Bad $ "Error: expression is not of type bool in the guard of an IF STATEMENT.")
        let thenEnv = pushContext env                   -- Entering the scope of true
        thenEnv' <- checkStm thenEnv s1
        let elseEnv = pushContext $ popContext thenEnv' -- Exiting the scope of true and entering the scope of false
        elseEnv' <- checkStm elseEnv s2
        return $ popContext elseEnv'                    -- Exiting the scope of false and return the new env

    While r s -> do
        tr <- inferRExp env r
        unless (tr == TBool) (EM.Bad $ "Error: expression is not of type bool in the guard of a WHILE STATEMENT.")
        let loopEnv = pushWhile env
        exitEnv <- checkStm loopEnv s
        return $ popContext exitEnv

    DoWhile s r -> do
        tr <- inferRExp env r
        unless (tr == TBool) (EM.Bad $ "Error: expression is not of type bool in the guard of a WHILE STATEMENT.")
        let loopEnv = pushWhile env
        exitEnv <- checkStm loopEnv s
        return $ popContext exitEnv


    For ident rng s -> do
        checkRange env rng
        let loopEnv  = pushFor env
        let loopEnv' = makeForCounter loopEnv ident
        exitEnv <- checkStm loopEnv s
        return $ popContext exitEnv

    
    JmpStm jmp -> do
        checkJmp env jmp
        return env


checkRange :: Env -> Range -> EM.Err Env
checkRange env rng = do
    ts <- inferRExp env $ start rng
    te <- inferRExp env $ end   rng
    unless (ts `subtypeOf` TInt) (EM.Bad "Error: START of a RANGE is not subtype of int.")
    unless (ts `subtypeOf` TInt) (EM.Bad "Error: END of a RANGE is not subtype of int.")
    return env

checkDecl :: Env -> Decl -> EM.Err Env
checkDecl env decl = EM.Bad $ "Error: CHECK for a Declaration not implemented yet."

checkBlock :: Env -> Block -> EM.Err Env
checkBlock env block = do
    let env1 = pushContext env
    env2 <- foldM checkDecl env1 (decls block)
    env3 <- foldM checkStm  env2 (stms  block)
    return $ popContext env3



-- CHECK VALIDITY OF JUMP STATEMENTS ///////////////////////////////////////////////////////////////////

checkJmp :: Env -> Jump -> EM.Err () -- To be changed to ET.Err ()
checkJmp env@(c:cs) jmp = case jmp of
    Return _    -> do
        when (inFor c || inWhile c) (EM.Bad "Cannot return inside a loop.")
        unless (returns c == TVoid) (EM.Bad "cannot return a type different from void in a PROCEDURE")

    ReturnE _ r -> do
        t <- inferRExp env r
        let t' = returns c
        when (inFor c || inWhile c) (EM.Bad "Cannot return inside a loop.")
        unless (t `subtypeOf` t') (EM.Bad ("the return type should be " ++ (show t')))

    Break _     -> do
        when (inFor c) (EM.Bad "Cannot use break inside a for statement.")
        unless (inWhile c) (EM.Bad "cannot use break outside a (do-)while statement.")
    
    Continue _  -> do
        when (inFor c) (EM.Bad "Cannot use continue inside a for-statement.")
        unless (inWhile c) (EM.Bad "cannot use continue outside a (do-)while statement.")
