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
import ErrorHandling
import PrintChapel

-- Copied from Skel --------------------------------------
type Result a = EM.Err a

failure :: Show a => a -> Result b
failure x = EM.Bad $ "Undefined case: " ++ show x
----------------------------------------------------------

predList :: [(String, Entry)]
predList = [
    ("readChar",    Fun (Loc 0 0) [] In TChar),
    ("readInt",     Fun (Loc 0 0) [] In TInt),
    ("readReal",    Fun (Loc 0 0) [] In TReal),
    ("readString",  Fun (Loc 0 0) [] In TString),
    ("writeChar",   Fun (Loc 0 0) [Param (Loc 0 1) "x" In TChar] In TVoid),
    ("writeInt",    Fun (Loc 0 0) [Param (Loc 0 1) "x" In TInt] In TVoid),
    ("writeReal",   Fun (Loc 0 0) [Param (Loc 0 1) "x" In TReal] In TVoid),
    ("writeString", Fun (Loc 0 0) [Param (Loc 0 1) "x" In TString] In TVoid)]

startEnv :: Env
startEnv = [Context (M.fromList predList) TVoid False False False]

typeCheck :: Program -> EM.Err Env
typeCheck = checkProgram startEnv

checkProgram :: Env -> Program -> EM.Err Env
checkProgram env (Prog decls) = ET.fromErrT $ do
    env1 <- foldM loadFunction env decls
    foldM checkDecl env1 decls


-- INFER RIGHT EXPRESSIONS //////////////////////////////////////////////////////////////////////////////

inferRExp :: Env -> RExp -> EM.Err TCType
inferRExp env rexp = case rexp of
    Or _ r1 r2 -> do
        t1 <- inferRExp env r1
        t2 <- inferRExp env r2
        unless (t1 == TBool) $ errorOr r1 t1
        unless (t2 == TBool) $ errorOr r2 t2
        return TBool

    And _ r1 r2 -> do
        t1 <- inferRExp env r1
        t2 <- inferRExp env r2
        unless (t1 == TBool) $ errorAnd r1 t1
        unless (t2 == TBool) $ errorAnd r2 t2
        return TBool
    
    Not _ r -> do
        t <- inferRExp env r
        unless (t == TBool) $ errorNot r t
        return TBool
        
    Comp _ r1 op r2 -> do
        t1 <- inferRExp env r1
        t2 <- inferRExp env r2
        let t = supremum t1 t2
        when (t == TError) $ errorBinary op r1 r2 t1 t2
        return TBool

    Arith _ r1 op r2 -> do
        t1 <- inferRExp env r1
        t2 <- inferRExp env r2
        let t = supremum t1 t2
        when (t == TError) $ errorBinary op r1 r2 t1 t2
        return t

    Sign _ _ r -> inferRExp env r

    RefE _ l -> do
        t <- inferLExp env l
        return $ TPoint t

    RLExp _ l -> do
        t <- inferLExp env l
        return t
    
    ArrList loc rs -> case rs of
        [] -> errorArrayEmpty loc
        _  -> do
            t <- foldl1 supremumM $ map (inferRExp env) rs
            when (t == TError) $ errorArrayElementsCompatibility loc
            return $ TArr (length rs) t
    
    FCall _ ident rs -> do
        ET.fromErrT $ checkCall env ident rs
        lookType ident env

    PredR _ pread -> do
        ET.fromErrT $ checkCall env (toIdent pread) []
        return $ tctypeOf pread

    Lit _ literal -> return $ tctypeOf literal


-- INFER LEFT EXPRESSIONS //////////////////////////////////////////////////////////////////////////////

inferLExp :: Env -> LExp -> EM.Err TCType
inferLExp env lexp = case lexp of
    Deref l     -> do
        t <- inferLExp env l
        case t of
            TPoint t' -> return t'
            _         -> errorNotAPointer lexp

    Access l r  -> do
        ta <- inferLExp env l
        ti <- inferRExp env r
        when (ti `supremum` TInt /= TInt) $ errorArrayIndex r
        case ta of
            TArr _ t -> return t
            _        -> errorArrayNot l

    Name ident  -> lookType ident env


-- INFER EXPRESSIONS WITH ERRORS

checkRExpError :: TCType -> Env -> RExp -> ET.ErrT TCType
checkRExpError = checkExpError inferRExp
    
checkLExpError :: TCType -> Env -> LExp -> ET.ErrT TCType
checkLExpError = checkExpError inferLExp

-- CHECK VALIDITY OF STATEMENTS /////////////////////////////////////////////////////////////////////////

checkStm :: Env -> Stm -> ET.ErrT Env
checkStm env stm = case stm of
    StmBlock block -> checkBlock env block

    StmCall ident rs -> checkCall env ident rs

    PredW pwrite r -> checkCall env (toIdent pwrite) [r]
    
    Assign l _ r -> do
        tl <- checkLExpError TError env l
        tr <- checkRExpError tl env r
        unlessT env (tr `subtypeOf` tl) $ errorAssignType $ locOf l
        unlessT env (isMutable env l)   $ errorAssignImmutable l
        -- whenT   env (isFunction env l)  $ errorAssignFunction $ locOf l
        return env

    StmL l -> do
        checkLExpError TVoid env l
        return env

    If r stm -> checkIf env r stm

    IfElse r s1 s2 -> checkIfElse env r s1 s2

    While r s -> checkWhile env r s

    DoWhile s r -> checkWhile env r s

    For ident rng s -> checkFor env ident rng s
    
    JmpStm jmp -> do
        checkJmp env jmp
        return env


checkPassing :: Env -> (RExp, Param) -> ET.ErrT Env
checkPassing env (r, Param loc id it tp) = do
    t <- ET.toErrT tp $ inferRExp env r
    case it of
        In          ->  unlessT env (t `subtypeOf` tp)  $ errorPassingTypeSub r t tp
        Out         -> (unlessT env (tp `subtypeOf` t)  $ errorPassingTypeSuper r t tp) >> 
                       (unlessT env (isLExp r)          $ errorPassingLExp r)
        InOut       -> (unlessT env (t == tp)           $ errorPassingTypeSame r t tp) >>
                       (unlessT env (isLExp r)          $ errorPassingLExp r)
        Ref         -> (unlessT env (t == tp)           $ errorPassingTypeSame r t tp) >>
                       (unlessT env (isLExp r)          $ errorPassingLExp r)
        ConstIn     ->  unlessT env (t `subtypeOf` tp)  $ errorPassingTypeSub r t tp
        ConstRef    -> (unlessT env (t == tp)           $ errorPassingTypeSame r t tp) >>
                       (unlessT env (isLExp r)          $ errorPassingLExp r)


checkCall :: Env -> Ident -> [RExp] -> ET.ErrT Env
checkCall env ident actuals = ET.toErrT env $ do
    f <- lookFun ident env
    let la = length actuals
        params = paramsOf f
        lp = length params
    unless (la == lp) $ errorCallWrongNumber ident la lp
    ET.fromErrT $ foldM checkPassing env $ zip actuals params



emptyBlock :: Block
emptyBlock = Block (Loc 0 0) [] []

checkIf :: Env -> RExp -> Stm -> ET.ErrT Env
checkIf env r s = checkIfElse env r s (StmBlock emptyBlock)

checkIfElse :: Env -> RExp -> Stm -> Stm -> ET.ErrT Env
checkIfElse env r s1 s2 = do
    tr <- checkRExpError TBool env r
    unlessT env (tr == TBool) $ errorGuard r tr
    let thenEnv = pushContext env                   -- Entering the scope of true
    thenEnv' <- checkStm thenEnv s1
    let elseEnv = pushContext $ popContext thenEnv' -- Exiting the scope of true and entering the scope of false
    elseEnv' <- checkStm elseEnv s2
    return $ popContext elseEnv'                    -- Exiting the scope of false and return the new env


checkWhile :: Env -> RExp -> Stm -> ET.ErrT Env
checkWhile env r s = do
    tr <- checkRExpError TBool env r
    unlessT env (tr == TBool) $ errorGuard r tr
    let loopEnv = pushWhile env
    exitEnv <- checkStm loopEnv s
    return $ popContext exitEnv


checkFor :: Env -> Ident -> Range -> Stm -> ET.ErrT Env
checkFor env ident rng s = do
    checkRange env rng
    let loopEnv  = pushFor env
    loopEnv' <- ET.toErrT loopEnv $ makeForCounter loopEnv ident
    exitEnv <- checkStm loopEnv' s
    return $ popContext exitEnv


checkRange :: Env -> Range -> ET.ErrT Env
checkRange env rng = do
    ts <- checkRExpError TInt env $ start rng
    te <- checkRExpError TInt env $ end rng
    unlessT env (ts `subtypeOf` TInt) $ errorRangeStart (locOf rng) (start rng) ts
    unlessT env (te `subtypeOf` TInt) $ errorRangeEnd (locOf rng) (end rng) te
    return env

checkDecl :: Env -> Decl -> ET.ErrT Env
checkDecl env decl = case decl of
    FDecl id forms it ty blk -> do
        let params  = map formToParam forms             -- create Params from Forms
            entries = map paramToEntry params           -- create EnvEntries from Params
            env1    = pushFun env (tctypeOf ty) it
        unlessT () (it==In || it==Ref) $ errorReturnIntent (locOf id) it
        let makeEntry' = \e p -> ET.toErrT e $ makeEntry e p -- return the old env if something goes wrong
        env2 <- foldM makeEntry' env1 $ zip [identFromParam p | p <- params] entries  -- fold the entry insertion given the list (id, entry)
        env3 <- checkBlock env2 blk
        return $ popContext env3

    CList cs -> do
        foldM checkCDecl env cs

    VList vs -> do
        foldM checkVDecl env vs



-- Extends the environment with a new compile-time constant (if possibile)
--  * If no duplicate declaration occures and type is correct and initializer is a compile-time constant, OkT (env++constant)
--  * Otherwise, BadT env
checkCDecl :: Env -> CDecl -> ET.ErrT Env
checkCDecl env c@(CDecl id t r) = ET.toErrT env $ let tc = tctypeOf t in do
    tr <- inferRExp env r                 -- Error purpose: check that r is semantically correct
    unless (tr `subtypeOf` tc) $ errorConstTypeMismatch id tc tr
    case constexpr env r of
        Nothing -> errorNotConst id r
        Just x  -> do
            makeConst env id x


-- Extends the environment with a new variable (if possible)
--  * If no duplicate declaration occures and type of initialization is correct, OkT (env++initialized) 
--  * If no duplicate declaration occures and type of initialization is incorrect, BadT (env++not_initialized)
--  * If duplicate declaration occures, BadT env
checkVDecl :: Env -> VDecl -> ET.ErrT Env
checkVDecl env v = case v of
    Solo id t   -> ET.toErrT env $ makeMutable env id (tctypeOf t)

    Init id t r -> let tc = tctypeOf t in ET.toErrTM env (makeMutable env id tc) $ do
        tr <- inferRExp env r
        unless (tr `subtypeOf` tc) $ errorVarTypeMismatch id tc tr
        makeMutable env id tc


-- Insert function in the scope (to be used once we enter in a new block to permit mutual-recursion)
--  * If no duplicate declaration occures, OkT (env++function)
--  * Otherwise, BadT env
loadFunction :: Env -> Decl -> ET.ErrT Env
loadFunction env d = ET.toErrT env $ case d of
    FDecl id forms it ty _  ->
        let params = map formToParam forms      -- formToParam is in Env.hs
        in makeFun env id params it $ tctypeOf ty
    
    _                       -> return env


-- * Push a new empty context
-- * Load all function names (to allow mutual-recursion)
-- * Check all declarations
-- * Check all statements
-- * Pop the new context
checkBlock :: Env -> Block -> ET.ErrT Env
checkBlock env block = do
    let env1 = pushContext env
    env2 <- foldM loadFunction env1 (decls block)
    env3 <- foldM checkDecl env2 (decls block)
    env4 <- foldM checkStm env3 (stms block)
    return $ popContext env4



-- CHECK VALIDITY OF JUMP STATEMENTS ///////////////////////////////////////////////////////////////////

checkJmp :: Env -> Jump -> ET.ErrT () -- To be changed to ET.Err ()
checkJmp env@(c:cs) jmp = ET.toErrT () $ case jmp of
    Return _    -> do
        when (inFor c || inWhile c) $ errorReturnLoop $ locOf jmp
        unless (returns c == TVoid) $ errorReturnProcedure (locOf jmp) $ returns c

    ReturnE _ r -> let t' = returns c in do
        t <- ET.fromErrT $ checkRExpError t' env r
        when (inFor c || inWhile c) $ errorReturnLoop $ locOf jmp
        unless (t `subtypeOf` t') $ errorReturnTypeMismatch (locOf jmp) t t'
        when ((isRef c) && (not (isLExp r))) $ errorReturnRef r

    Break _     -> do
        when (inFor c) $ errorBreakFor $ locOf jmp
        unless (inWhile c) $ errorBreakOutside $ locOf jmp
    
    Continue _  -> do
        when (inFor c) $ errorContinueFor $ locOf jmp
        unless (inWhile c) $ errorContinueOutside $ locOf jmp
