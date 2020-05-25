module TypeChecker where

import AbsChapel
import TCType
import TCInstances
import Locatable
import Env
import Control.Monad (guard, unless, when, foldM)
import qualified ErrM as EM
import qualified ErrT as ET
import CompileTime
import qualified Data.Map.Lazy as M
import Data.Char

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
        unless (t1 == TBool) (EM.Bad ("Error at line " ++ (show (linOf r1)) ++ ": the operand " ++ (show r1) ++ " in an OR expression should have type bool, instead has type " ++ (show t1)))
        unless (t2 == TBool) (EM.Bad ("Error at line " ++ (show (linOf r2)) ++ ": the operand " ++ (show r2) ++ " in an OR expression should have type bool, instead has type " ++ (show t2)))
        return TBool

    And _ r1 r2 -> do
        t1 <- inferRExp env r1
        t2 <- inferRExp env r2
        unless (t1 == TBool) (EM.Bad ("Error at line " ++ (show (linOf r1)) ++ ": the operand " ++ (show r1) ++ " in an AND expression should have type bool, instead has type " ++ (show t1)))
        unless (t2 == TBool) (EM.Bad ("Error at line " ++ (show (linOf r2)) ++ ": the operand " ++ (show r2) ++ " in an AND expression should have type bool, instead has type " ++ (show t2)))
        return TBool
    
    Not _ r -> do
        t <- inferRExp env r
        unless (t == TBool) (EM.Bad ("Error at line " ++ (show (linOf r)) ++ ": the operand " ++ (show r) ++ " in a NOT expression should have type bool, instead has type " ++ (show t)))
        return TBool
        
    Comp _ r1 _ r2 -> do
        t1 <- inferRExp env r1
        t2 <- inferRExp env r2
        let t = supremum t1 t2
        when (t == TError) (EM.Bad ("Error at line " ++ (show (linOf r1)) ++ "operands " ++ (show r1) ++ " (type: " ++ (show t1) ++ ") and " ++ (show r2) ++ " (type: " ++ (show t2) ++ ") are not compatible in a COMPARISON expression"))
        return TBool

    Arith _ r1 _ r2 -> do
        t1 <- inferRExp env r1
        t2 <- inferRExp env r2
        let t = supremum t1 t2
        when (t == TError) (EM.Bad ("Error at line " ++ (show (linOf r1)) ++ "operands " ++ (show r1) ++ " (type: " ++ (show t1) ++ ") and " ++ (show r2) ++ " (type: " ++ (show t2) ++ ") are not compatible in an ARITHMETIC expression"))
        return t

    Sign _ _ r -> inferRExp env r

    RefE _ l -> do
        t <- inferLExp env l
        return $ TPoint t

    RLExp _ l -> do
        t <- inferLExp env l
        return t
    
    ArrList _ rs -> case rs of
        [] -> EM.Bad ("Error at line " ++ (show (linOf rexp)) ++ ": empty array initializer")
        _  -> do
            t <- foldl1 supremumM $ map (inferRExp env) rs
            when (t == TError) (EM.Bad ("Error at line " ++ (show (linOf rexp)) ++ ": types in the array initializer " ++ (show rs) ++ " are not compatible"))
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
            _         -> EM.Bad $ ("Error at line " ++ (show (linOf l)) ++ ": trying to dereference the non-pointer variable " ++ (show l))

    Access l r  -> do
        ta <- inferLExp env l
        ti <- inferRExp env r
        when (ti `supremum` TInt /= TInt) (EM.Bad ("Error at line " ++ (show (linOf r)) ++ ": array index " ++ (show r) ++ " should have tipe integer in an ARRAY ACCESS"))
        case ta of
            TArr _ t -> return t
            _        -> EM.Bad $ ("Error at line " ++ (show (linOf l)) ++ ": left expression " ++ (show l) ++ " is not an array in an ARRAY ACCESS")

    Name ident  -> lookType (idName ident) env


-- INFER EXPRESSIONS WITH ERRORS

checkRExpError :: Env -> RExp -> EM.Err TCType
checkRExpError env rexp = let t = inferRExp env rexp in case t of
    (EM.Ok t') -> return t'
    (EM.Bad s) -> (EM.Bad (s ++ ", in the expression " ++ (show rexp)))
    
checkLExpError :: Env -> LExp -> EM.Err TCType
checkLExpError env lexp = let t = inferLExp env lexp in case t of
    (EM.Ok t') -> return t'
    (EM.Bad s) -> (EM.Bad (s ++ ", in the expression " ++ (show lexp)))

-- CHECK VALIDITY OF STATEMENTS /////////////////////////////////////////////////////////////////////////

checkStm :: Env -> Stm -> EM.Err Env -- To be changed to ET.Err ()
checkStm env stm = let x = "Dummy" in case stm of -- Dummy x to be deleted once completed all patterns
    StmBlock block -> checkBlock env block

    StmCall ident rs -> do
        f <- lookFun (idName ident) env -- @TODO: check that arguments are ok with function definition
        return env

    PredW pwrite r -> do
        let t = tctypeOf pwrite
        t' <- checkRExpError env r    
        unless (t' `subtypeOf` t) (EM.Bad $ "Error: type mismatch in a FUNCTION CALL.")
        return env
    
    Assign l _ r -> do -- @TODO: check that l is non-constant (and non-function)
        tl <- checkLExpError env l
        tr <- checkRExpError env r
        unless (tr `subtypeOf` tl) (EM.Bad $ "Error: type mismatch in an ASSIGNMENT.")
        return env

    StmL l -> do
        inferLExp env l
        return env

    If r stm -> do
        tr <- checkRExpError env r
        unless (tr == TBool) (EM.Bad $ "Error: expression is not of type bool in the guard of an IF STATEMENT.")
        let thenEnv = pushContext env                   -- Entering the scope of true
        thenEnv' <- checkStm thenEnv stm
        return $ popContext thenEnv'                    -- Exiting the scope of true and return the new env

    IfElse r s1 s2 -> do
        tr <- checkRExpError env r
        unless (tr == TBool) (EM.Bad $ "Error: expression is not of type bool in the guard of an IF STATEMENT.")
        let thenEnv = pushContext env                   -- Entering the scope of true
        thenEnv' <- checkStm thenEnv s1
        let elseEnv = pushContext $ popContext thenEnv' -- Exiting the scope of true and entering the scope of false
        elseEnv' <- checkStm elseEnv s2
        return $ popContext elseEnv'                    -- Exiting the scope of false and return the new env

    While r s -> do
        tr <- checkRExpError env r
        unless (tr == TBool) (EM.Bad $ "Error: expression is not of type bool in the guard of a WHILE STATEMENT.")
        let loopEnv = pushWhile env
        exitEnv <- checkStm loopEnv s
        return $ popContext exitEnv

    DoWhile s r -> do
        tr <- checkRExpError env r
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
checkDecl env decl = case decl of
    FDecl _ _ _ _ _ -> do
        let params  = map formToParam forms             -- create Params from Forms
        let entries = map paramToEntry params           -- create EnvEntries from Params
        env' <- foldM makeEntry env $ zip [id | (Param _ id _ _) <- params] entries  -- fold the entry insertion given the list (id, entry)
        checkBlock env' blk

    CList cs -> do
        foldM checkCDecl env cs

    VList vs -> do
        foldM checkVDecl env vs


checkCDecl :: Env -> CDecl -> EM.Err Env
checkCDecl env c@(CDecl id t r) = do
    inferRExp env r                 -- Error purpose: check that r is well-typed
    case constexpr env r of
        Nothing -> (EM.Bad "Error: the expression is not a const-expression in a CONST DECLARATION.")
        Just x  -> makeConst env id x


checkVDecl :: Env -> VDecl -> EM.Err Env
checkVDecl env v = case v of
    Solo id t   -> makeMutable env id (tctypeOf t)

    Init id t r -> do
        let tc = tctypeOf t
        tr <- inferRExp env r
        unless (tr `subtypeOf` tc) (EM.Bad "Error: type mismatch in a VAR DECLARATION.")
        makeMutable env id tc


-- Returns a literal if r is a compile-time constant expression,
-- Nothing otherwise.
-- (Precondition): expected r to be well-typed
constexpr :: Env -> RExp -> Maybe Literal
constexpr env r = case r of
    Or _ r1 r2 -> do
        (LBool b1) <- constexpr env r1
        (LBool b2) <- constexpr env r2
        return . LBool $ b1 || b2

    And _ r1 r2 -> do
        (LBool b1) <- constexpr env r1
        (LBool b2) <- constexpr env r2
        return . LBool $ b1 && b2
    
    Not _ r -> do
        (LBool b) <- constexpr env r
        return . LBool $ not b
        
    Comp _ r1 op r2 -> do
        l1 <- constexpr env r1
        l2 <- constexpr env r2
        b  <- litComp op l1 l2
        return . LBool $ b

    Arith _ r1 op r2 -> do
        l1 <- constexpr env r1
        l2 <- constexpr env r2
        litArith op l1 l2

    Sign _ Pos r -> constexpr env r

    Sign _ Neg r -> do
        l <- constexpr env r
        case l of
            LChar a -> return . LInt . toInteger $ -(ord a)
            LInt  a -> return . LInt $ -a
            LReal a -> return . LReal $ -a
            _       -> Nothing

    RLExp _ l -> constexprL env l
    
    ArrList _ rs -> do
        let lits = map (constexpr env) rs                -- take list of Maybe literals recursively
        let t    = foldl1 supremum $ map tctypeOf lits   -- take unifier type
        guard (t /= TError)                              -- safety check

        let conv = case t of                             -- select coercion function based on more general type in lits
                    TInt    -> toLInt
                    TReal   -> toLReal
                    TString -> toLString
                    _       -> id

        return $ LArr [ conv x | (Just x) <- lits ]

    Lit _ lit   -> Just lit

    _ -> Nothing


constexprL :: Env -> LExp -> Maybe Literal
constexprL env lexp = case lexp of
    Deref l     -> Nothing

    Access l r  -> do
        arr <- constexprL env l     -- take literal array
        i <- constexpr env r        -- take literal index
        litAccess arr i             -- return the accessed value

    Name ident  -> do
        (Const _ _ lit) <- EM.errToMaybe $ lookConst (idName ident) env
        return lit


-- Insert function in the scope (to be used once we enter in a new block to permit mutual-recursion)
loadFunction :: Env -> Decl -> EM.Err Env
loadFunction env d = case d of
    FDecl id forms it ty _  ->
        let params = map formToParam forms      -- formToParam is in Env.hs
        in makeFun env id params it $ tctypeOf ty
    
    _                       -> return env

checkBlock :: Env -> Block -> EM.Err Env
checkBlock env block = do
    let env1 = pushContext env
    env2 <- foldM loadFunction env1 (decls block)
    env3 <- foldM checkDecl env2 (decls block)
    env4 <- foldM checkStm  env3 (stms  block)
    return $ popContext env4



-- CHECK VALIDITY OF JUMP STATEMENTS ///////////////////////////////////////////////////////////////////

checkJmp :: Env -> Jump -> EM.Err () -- To be changed to ET.Err ()
checkJmp env@(c:cs) jmp = case jmp of
    Return _    -> do
        when (inFor c || inWhile c) (EM.Bad "Cannot return inside a loop.")
        unless (returns c == TVoid) (EM.Bad "cannot return a type different from void in a PROCEDURE")

    ReturnE _ r -> do
        t <- checkRExpError env r
        let t' = returns c
        when (inFor c || inWhile c) (EM.Bad "Cannot return inside a loop.")
        unless (t `subtypeOf` t') (EM.Bad ("the return type should be " ++ (show t')))

    Break _     -> do
        when (inFor c) (EM.Bad "Cannot use break inside a for statement.")
        unless (inWhile c) (EM.Bad "cannot use break outside a (do-)while statement.")
    
    Continue _  -> do
        when (inFor c) (EM.Bad "Cannot use continue inside a for-statement.")
        unless (inWhile c) (EM.Bad "cannot use continue outside a (do-)while statement.")
