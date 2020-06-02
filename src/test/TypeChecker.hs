module TypeChecker where

import AbsChapel
import TCType
import TCInstances
import Locatable
import Env
import Control.Monad (guard, unless, when, foldM, liftM2)
import qualified ErrM as EM
import qualified ErrT as ET
import CompileTime
import qualified Data.Map.Lazy as M
import Data.Char
import ErrorHandling
import PrintChapel


-- List of predefined functions
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

-- Initial environment (with the global context)
startEnv :: Env
startEnv = [Context (M.fromList predList) TVoid False False False]

-- Typecheck starting point: check of a program with the startEnv
typeCheck :: Program -> EM.Err Program
typeCheck = checkProgram startEnv


-- Function that abstracts a process needed to foldM other functions (checkDecl, checkCDecl, checkVDecl, etc...)
foldWrapper :: (Env -> a -> ET.ErrT (Env, a)) -> ((Env, [a]) -> a -> ET.ErrT (Env, [a]))
foldWrapper f = 
    \(e, ls) a -> do
        (e', a') <- f e a
        return (e', a' : ls)

-- foldWrapper for checkStm
foldWrapper' :: (Env -> a -> ET.ErrT a) -> ((Env, [a]) -> a -> ET.ErrT (Env, [a]))
foldWrapper' f =
    \(e, ls) a -> do
        a' <- f e a
        return (e, a' : ls)


-- Loading of function names and check of all declarations
checkProgram :: Env -> Program -> EM.Err Program
checkProgram env (Prog decls) = ET.fromErrT $ do
    env1 <- foldM loadFunction env decls
    eds <- foldM (foldWrapper checkDecl) (env1, []) decls
    return . Prog . reverse $ snd eds       -- decls were consed in reverse


-- Insert function name in the scope (to be used once we enter in a new block to permit mutual-recursion)
--  * If no duplicate declaration occures, OkT (env++function)
--  * Otherwise, BadT env
loadFunction :: Env -> Decl -> ET.ErrT Env
loadFunction env d = ET.toErrT env $ case d of
    FDecl id forms it ty _  ->
        let params = map formToParam forms      -- formToParam is in Env.hs
        in makeFun env id params it $ tctypeOf ty
    
    _                       -> return env


-- Check of a declaration: for variables and compile-time constants lists just fold of an individual check
-- For a function, first we filter-out unreachable statements and then we check the "filtered" function
checkDecl :: Env -> Decl -> ET.ErrT (Env, Decl)
checkDecl env decl = case decl of
    FDecl id forms it rt b ->
        let ss  = stms b
            ss' = reachables ss
            b'  = Block (locOf b) (decls b) ss'
        in  checkFDecl env (FDecl id forms it rt b')

    CList cs -> do
        (env', cs') <- foldM (foldWrapper checkCDecl) (env, []) cs
        return (env', CList cs')

    VList vs -> do
        (env', vs') <- foldM (foldWrapper checkVDecl) (env, []) vs
        return (env', VList vs')


-- Given a list of statements, returns a list of statements filtering out unreachable ones
-- A statement s is unreachable when there is a "sink" between the start statement and s
-- A sink is a statement that terminates the computation in every possible execution paths:
--   * 'return' and 'return expr' are sinks
--   * if-else is a sink when both branches are sinks
--   * a block is a sink when it contains a sink
reachables :: [Stm] -> [Stm]
reachables ss = ss'
    where
        extends = \s -> case s of   -- Transform a block in a block of reachable statements
            StmBlock b  -> StmBlock $ Block (locOf b) (decls b) (reachables $ stms b)
            _           -> s
        
        isSink = \s -> case s of    -- Predicate to capture the notion of sink
            JmpStm (Return _)       -> True
            JmpStm (ReturnE _ _)    -> True
            IfElse _ s1 s2          -> isSink s1 && isSink s2
            StmBlock b              -> or $ map isSink $ stms b
            _                       -> False

        extended = map extends ss           -- "Relax" all blocks
        (n, y)   = break isSink extended    -- split statements on the first sink
        ss'      = n ++ (take 1 y)          -- Take only the reachable ones (i.e. statements until first sink included)


-- Check for the "totality" of a list of statements
-- (although the definition of totality for a statement is a bit stretched)
--
--   * a return statement is total
--   * an if-else is total if both his branches are total
--   * a block is total if the list of statements inside it is total
--   * all other statements are not total
--
-- The entire list is total if we reach a total statement from the first one:
-- the reachability on the graph of statements is emulated by a fold right (with base case False)
checkReturnPaths :: [Stm] -> Bool
checkReturnPaths ls = foldr helper False ls
    where
        helper = \s acc -> acc || case s of
            JmpStm (Return _)       -> True
            JmpStm (ReturnE _ _)    -> True
            IfElse _ s1 s2          -> (checkReturnPaths [s1]) && (checkReturnPaths [s2])
            StmBlock b              -> checkReturnPaths $ stms b
            _                       -> False


-- Check that a parameters passed by res/valres are assigned to a value in all execution paths
checkRes :: Env -> Ident -> [Param] -> [Stm] -> ET.ErrT ()
checkRes env id params ss = foldl (>>) (return ()) errs
    where
        errs    = map ferr params
        ferr    = \x@(Param _ n _ _) ->     -- Given a Param x, set error when at the end there is an exec path where it doesn't have a value assigned
                    if helper x ss
                    then return ()
                    else ET.toErrT () $ errorMissingAssignRes (locOf x) (idName id) n
        helper  = \x@(Param _ n _ _) ss ->
            let helper' = \s acc -> case s of   -- Roughly the same structure of return-paths
                    JmpStm (Return _)       -> False
                    JmpStm (ReturnE _ _)    -> False
                    Assign (Name ident) _ _ -> acc || (idName ident) == n
                    IfElse _ s1 s2          -> acc || (helper x [s1]) && (helper x [s2])
                    StmBlock b              -> acc || (helper x $ stms b)
                    _                       -> acc
            in foldr helper' False ss

-- Check for a function definition:
--   * return intent must be either In (by value) or Ref (by reference)
--   * Proper functions must reach a 'return' in every possibile exec path
--   * check that all res/valres parameters will reach an assignment in every exec path
--   * check that the body with the formal parameters added in is valid
checkFDecl :: Env -> Decl -> ET.ErrT (Env, Decl)
checkFDecl env (FDecl id forms it ty blk) =
    let params          = map formToParam forms             -- create Params from Forms
        resCandidates   = filter (\(Param _ _ it _) -> it == Out || it == InOut) params -- Get params passed by res/valres
        entries         = map paramToEntry params           -- create EnvEntries from Params
        env1            = pushFun env (tctypeOf ty) it
        makeEntry' = \e p -> ET.toErrT e $ makeEntry e p -- return the old env if something goes wrong
    in do
        unlessT () (it==In || it==Ref) $ errorReturnIntent (locOf id) it
        whenT ()  (tctypeOf ty /= TVoid && (not $ checkReturnPaths $ stms blk)) $ errorReturnMissing id
        checkRes env1 id resCandidates $ stms blk
        env2 <- foldM makeEntry' env1 $ zip [identFromParam p | p <- params] entries  -- fold the entry insertion given the list (id, entry)
        (env3, blk') <- checkScope env2 blk
        return (popContext env3, FDecl id forms it ty blk')


-- Extends the environment with a new compile-time constant (if possibile)
--  * If no duplicate declaration occures and type is correct and initializer is a compile-time constant, OkT (env++constant)
--  * Otherwise, BadT env
--
-- constexpr is defined in module CompileTime
checkCDecl :: Env -> CDecl -> ET.ErrT (Env, CDecl)
checkCDecl env c@(CDecl id t r) = ET.toErrT (env, c) $ let tc = tctypeOf t in do
    (tr, r') <- inferRExp env r                 -- Error purpose: check that r is semantically correct
    unless (tr `subtypeOf` tc) $ errorConstTypeMismatch id tc tr
    case constexpr env r of
        Nothing -> errorNotConst id r
        Just x  -> do
            env' <- makeConst env id x
            return (env', CDecl id t r')


-- Extends the environment with a new variable (if possible)
--  * If no duplicate declaration occures and type of initialization is correct, OkT (env++initialized) 
--  * If no duplicate declaration occures and type of initialization is incorrect, BadT (env++not_initialized)
--  * If duplicate declaration occures, BadT env
checkVDecl :: Env -> VDecl -> ET.ErrT (Env, VDecl)
checkVDecl env v = case v of
    Solo id t   -> do
        env' <- ET.toErrT env $ makeMutable env id (tctypeOf t)
        return (env', Solo id t)

    Init id t r -> 
        let tc = tctypeOf t 
            mk = makeMutable env id tc
        in case mk of
            EM.Ok env'  -> ET.toErrT (env', Solo id t) $ do
                (tr, r') <- inferRExp env r
                unless (tr `subtypeOf` tc) $ errorVarTypeMismatch id tc tr
                return (env', Init id t r')
            EM.Bad s    -> ET.badT (env, v) s



-- Type inference for right expressions: all cases are delegated to minor functions
inferRExp :: Env -> RExp -> EM.Err (TCType, RExp)
inferRExp env rexp = case rexp of
    Or _ _ _        -> inferOr env rexp
    And _ _ _       -> inferAnd env rexp
    Not _ _         -> inferNot env rexp
    Comp _ _ _ _    -> inferComp env rexp
    Arith _ _ _ _   -> inferArith env rexp
    Sign _ _ _      -> inferSign env rexp
    RefE _ _        -> inferRefE env rexp
    RLExp _ _       -> inferRLExp env rexp
    ArrList _ _     -> inferArrList env rexp
    FCall _ _ _     -> inferFCall env rexp
    Lit _ _         -> inferLit rexp


-- Infer logical operators //////////////////////////////////////////////////////////

inferOr :: Env -> RExp -> EM.Err (TCType, RExp)
inferOr env (Or l r1 r2) = do
    r1' <- inferLogicalOperand errorOr env r1
    r2' <- inferLogicalOperand errorOr env r2
    return (TBool, Or l r1' r2')

inferAnd :: Env -> RExp -> EM.Err (TCType, RExp)
inferAnd env (And l r1 r2) = do
    r1' <- inferLogicalOperand errorAnd env r1
    r2' <- inferLogicalOperand errorAnd env r2
    return (TBool, And l r1' r2')

inferNot :: Env -> RExp -> EM.Err (TCType, RExp)
inferNot env (Not l r) = do
    r' <- inferLogicalOperand errorNot env r
    return (TBool, Not l r')

-- Abstraction on the single logical operand inference
inferLogicalOperand :: (RExp -> TCType -> EM.Err ()) -> Env -> RExp -> EM.Err RExp
inferLogicalOperand errF env r = do
    (t, r') <- inferRExp env r
    unless (t == TBool) $ errF r t
    return r'


-- Infer relational and arithmetic operators ///////////////////////////////////////////////////////////


-- For relational operators we need both operands to be compatible with each other
inferComp :: Env -> RExp -> EM.Err (TCType, RExp)
inferComp env (Comp loc r1 op r2) = do
    (t1, r1') <- inferRExp env r1
    (t2, r2') <- inferRExp env r2
    when (supremum t1 t2 == TError) $ errorBinary op r1 r2 t1 t2
    return (TBool, Comp loc r1' op r2')

-- For arithmetic operators we need "numeric" operands (i.e. subtype of real)
-- extra conditions for '%' (operands both subtype of int) and '^' (exponent subtype of int)
inferArith :: Env -> RExp -> EM.Err (TCType, RExp)
inferArith env (Arith loc r1 op r2) = do
    (t1, r1') <- inferRExp env r1
    (t2, r2') <- inferRExp env r2
    let t = supremum t1 t2
    when (t == TError) $ errorBinary op r1 r2 t1 t2
    unless (t `subtypeOf` TReal) $ errorArithmetic op r1 r2 t1 t2
    case op of
        Mod ->  (unless (t1 `subtypeOf` TInt) $ errorArithOperandInt r1 t1) >>
                (unless (t2 `subtypeOf` TInt) $ errorArithOperandInt r2 t2)
        Pow ->   unless (t2 `subtypeOf` TInt) $ errorArithOperandInt r2 t2
        _   ->  return ()
    return (t, Arith loc r1' op r2')


inferSign :: Env -> RExp -> EM.Err (TCType, RExp)
inferSign env rexp@(Sign loc op r) = do
    (t, r') <- inferRExp env r
    unless (t `subtypeOf` TReal) $ errorSignNotNumber rexp t
    return (t, Sign loc op r')

-- ArrayList type is the supremum of the inferred types inside the list
--   * Empty array throws an error
--   * Non compatible types throw an error
inferArrList :: Env -> RExp -> EM.Err (TCType, RExp)
inferArrList env (ArrList loc rs) = case rs of
    [] -> errorArrayEmpty loc
    _  -> do
        let mapped = map (inferRExp env) rs
            tsM    = map (fst <$>) mapped
            rsM    = map (snd <$>) mapped
        t <- foldl1 (liftM2 supremum) tsM
        when (t == TError) $ errorArrayElementsCompatibility loc
        let rs'    = map EM.fromOk rsM
        return (TArr (length rs) t, ArrList loc rs')

-- Reference to a left-expression have type pointer to..
inferRefE :: Env -> RExp -> EM.Err (TCType, RExp)
inferRefE env (RefE loc l) = do
    (t, l') <- inferLExp env l
    return (TPoint t, RefE loc l')

-- Right-left expressions have type of the left expression
inferRLExp :: Env -> RExp -> EM.Err (TCType, RExp)
inferRLExp env (RLExp loc l) = do
    (tl, l') <- inferLExp env l
    return (tl, RLExp loc l')


-- Function calls have type of the return
--   * Check of well formed call is delegated to checkCall
inferFCall :: Env -> RExp -> EM.Err (TCType, RExp)
inferFCall env (FCall loc ident rs) = do
    let cc = checkCall env ident rs
    rs' <- ET.fromErrT cc
    rt  <- lookType ident env
    return (rt, FCall loc ident rs')


-- Type of a literal is induced by the typeclass instance
inferLit :: RExp -> EM.Err (TCType, RExp)
inferLit lit@(Lit _ l) = return (tctypeOf l, lit)



-- Infer of a left-expression: all cases are delegated to minor inferers
inferLExp :: Env -> LExp -> EM.Err (TCType, LExp)
inferLExp env lexp = case lexp of
    Deref _     -> inferDeref env lexp
    Access _ _  -> inferAccess env lexp
    Name _      -> inferName env lexp


-- Type of a dereference is induced by the type of the pointer
--   * If the left-expression is not a pointer, throw an error
inferDeref :: Env -> LExp -> EM.Err (TCType, LExp)
inferDeref env (Deref l) = do
    (t, l') <- inferLExp env l
    case t of
        TPoint t' -> return (t', Deref l')
        _         -> errorNotAPointer l


-- Type of an array access is induced by the type of the array
--   * If the left-expression is not an array, throw an error
--   * If the right-expression is not subtype of an integer, throw an error
inferAccess :: Env -> LExp -> EM.Err (TCType, LExp)
inferAccess env (Access l r) = do
    (ta, l') <- inferLExp env l
    (ti, r') <- inferRExp env r
    when (ti `supremum` TInt /= TInt) $ errorArrayIndex r
    case ta of
        TArr _ t -> return (t, Access l' r')
        _        -> errorArrayNot l


-- Type of a name is induced by it's entry in the environment
--   * If the name does not exist, Env.lookType will throw an error
inferName :: Env -> LExp -> EM.Err (TCType, LExp)
inferName env (Name ident) = do
    t <- lookType ident env
    return (t, Name ident)



-- Infer right/left-expressions enhancing the error message:
-- this is used when we need to infer an expression in a check of a statement
-- 'checkExpError' is defined in module ErrorHandling
checkRExpError :: Env -> RExp -> EM.Err (TCType, RExp)
checkRExpError = checkExpError inferRExp
    
checkLExpError :: Env -> LExp -> EM.Err (TCType, LExp)
checkLExpError = checkExpError inferLExp



-- Check validity of a statement, given an environment: all cases are delegated to minor checkers
checkStm :: Env -> Stm -> ET.ErrT Stm
checkStm env stm = case stm of
    StmBlock _      -> checkBlock env stm
    StmCall _ _     -> checkStmCall env stm
    Assign _ _ _    -> checkAssign env stm
    StmL _          -> checkStmL env stm
    If _ _          -> checkIf env stm
    IfElse _ _ _    -> checkIfElse env stm
    While _ _       -> checkWhile env stm
    DoWhile _ _     -> checkDoWhile env stm
    For _ _ _       -> checkFor env stm
    JmpStm _        -> checkJmpStm env stm


-- Empty block
emptyBlock :: Block
emptyBlock = Block (Loc 0 0) [] []

-- Check of a block statement:
-- * Push a new empty context
-- * Check the new scope created
-- * Pop the new context
checkBlock :: Env -> Stm -> ET.ErrT Stm
checkBlock env (StmBlock block) = do
    let env1 = pushContext env
    (env2, block') <- checkScope env1 block
    return . StmBlock $ block'

-- Check of a scope:
-- * Load all function names (to allow mutual recursion)
-- * Add al new declared variables/constants
-- * Check the list of statements
checkScope :: Env -> Block -> ET.ErrT (Env, Block)
checkScope env block = do
    env1 <- foldM loadFunction env (decls block)
    (env2, ds) <- foldM (foldWrapper checkDecl) (env1, []) (decls block)
    (env3, ss) <- foldM (foldWrapper' checkStm) (env2, []) (stms block)
    return (env, Block (locOf block) (reverse ds) (reverse ss))


-- Helper for check of a function call:
-- check that the expression passed is compatible with the parameter type and intent
checkPassing :: Env -> (RExp, Param) -> ET.ErrT Env
checkPassing env (r, Param loc id it tp) = do
    (t, r') <- ET.toErrT (tp, r) $ inferRExp env r
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


-- Check of a function call:
--   * Function must exist in the scope, otherwise Env.lookFun will throw an error
--   * If actual and formal parameters lists have not the same length, throw an error
--   * If actual and formal types and intents are not compatible, throw an error
checkCall :: Env -> Ident -> [RExp] -> ET.ErrT [RExp]
checkCall env ident actuals = ET.toErrT actuals $ do
    f <- lookFun ident env
    let la = length actuals
        params = paramsOf f
        lp = length params
    unless (la == lp) $ errorCallWrongNumber ident la lp
    ET.fromErrT $ foldM checkPassing env $ zip actuals params
    return actuals

checkStmCall :: Env -> Stm -> ET.ErrT Stm
checkStmCall env (StmCall ident actuals) = do
    rs <- checkCall env ident actuals
    return $ StmCall ident rs


-- Check an assignment statement
--   * If the left-expression is ill-formed, throw an error
--   * If the right-expression is ill-formed, throw an error
--   * If not (leftType >= rightType), throw an error
--   * If left-expression is immutable, throw an error
--   * Special check for '%=' and '^='
--   * Right-expression must be a 'number' (subtype of real) when non-eq assignments are used
--   * Function-names cannot be on the left of an assignment
checkAssign :: Env -> Stm -> ET.ErrT Stm
checkAssign env stm@(Assign l op r) = ET.toErrT stm $ do
    (tl, l') <- checkLExpError env l
    (tr, r') <- checkRExpError env r
    unless (tr `subtypeOf` tl) $ errorAssignType $ locOf l
    unless (isMutable env l)   $ errorAssignImmutable l
    case op of
        AssignMod _ -> unless (tl `subtypeOf` TInt) $ errorAssignMod l tl
        AssignPow _ -> unless (tr `subtypeOf` TInt) $ errorAssignPow r tr
        AssignEq  _ -> return ()
        _           -> unless (tr `subtypeOf` TReal) $ errorAssignNotReal r tr
    when (isFunction env l)  $ errorAssignFunction $ locOf l
    return $ Assign l' op r'

-- Check a left-expression statement
--   * If the expression is ill-formed, throw an error
checkStmL :: Env -> Stm -> ET.ErrT Stm
checkStmL env stm@(StmL l) = ET.toErrT stm $ do
    (tl, l') <- checkLExpError env l
    return $ StmL l'


-- Check of if-then statement is delegated to if-then-else with an empty else-block
checkIf :: Env -> Stm -> ET.ErrT Stm
checkIf env (If r s) = do
    (r', s1', s2') <- ifHelper env r s (StmBlock emptyBlock)
    return $ If r' s1'

-- Check of an if-then-else statement:
--   * If the guard-expression is ill-formed, throw an error
--   * If the guard-expression is not a bool, throw an error
checkIfElse :: Env -> Stm -> ET.ErrT Stm
checkIfElse env (IfElse r s1 s2) = do
    (r', s1', s2') <- ifHelper env r s1 s2
    return $ IfElse r' s1' s2'

ifHelper :: Env -> RExp -> Stm -> Stm -> ET.ErrT (RExp, Stm, Stm)
ifHelper env r s1 s2 = do
    (tr, r') <- ET.toErrT (TBool, r) $ checkRExpError env r
    unlessT () (tr == TBool) $ errorGuard r tr
    let thenEnv = pushContext env                   -- Entering the scope of then
    s1' <- checkStm thenEnv s1
    let elseEnv = pushContext $ popContext thenEnv -- Exiting the scope of then and entering the scope of else
    s2' <- checkStm elseEnv s2
    return (r', s1', s2')


-- Check of a while-statement:
--   * If the guard-expression is ill-formed, throw an error
--   * If the guard-expression is not a bool, throw an error
checkWhile :: Env -> Stm -> ET.ErrT Stm
checkWhile env (While r s) = do
    (r', s') <- loopHelper env r s
    return $ While r' s'                     -- Exiting the loop scope


-- Do-while is delegated to checkWhile 
checkDoWhile :: Env -> Stm -> ET.ErrT Stm
checkDoWhile env (DoWhile s r) = do
    (r', s') <- loopHelper env r s
    return $ DoWhile s' r'

loopHelper :: Env -> RExp -> Stm -> ET.ErrT (RExp, Stm)
loopHelper env r s = do
    (tr, r') <- ET.toErrT (TBool, r) $ checkRExpError env r
    unlessT () (tr == TBool) $ errorGuard r tr
    let loopEnv = pushWhile env                     -- Entering the loop scope
    s' <- checkStm loopEnv s
    return (r', s')


-- check of for-statement:
--   * If range is ill-formed, throw an error
checkFor :: Env -> Stm -> ET.ErrT Stm
checkFor env (For ident rng s) = do
    rng' <- checkRange env rng
    let loopEnv  = pushFor env
    loopEnv' <- ET.toErrT loopEnv $ makeForCounter loopEnv ident
    s' <- checkStm loopEnv' s
    return $ For ident rng' s'


-- check of a for-range:
--   * If start/end expressions are ill-formed, throw an error
--   * If start/end expression are not a subtype of integer, throw an error
checkRange :: Env -> Range -> ET.ErrT Range
checkRange env (Range loc st en) = do
    (ts, st') <- ET.toErrT (TInt, st) $ checkRExpError env st
    (te, en') <- ET.toErrT (TInt, st) $ checkRExpError env en
    unlessT () (ts `subtypeOf` TInt) $ errorRangeStart loc st ts
    unlessT () (te `subtypeOf` TInt) $ errorRangeEnd loc en te
    return $ Range loc st' en'


-- Check of a jump statement is delegated
checkJmpStm :: Env -> Stm -> ET.ErrT Stm
checkJmpStm env (JmpStm jmp) = do
    jmp' <- checkJmp env jmp
    return $ JmpStm jmp'

-- * 'return;' cannot be used inside a loop
-- * 'return;' can be used only in procedures
--
-- * The returned expression in 'return expr;' must be well-formed
-- * 'return expr;' cannot be used inside a loop
-- * The returned expression in 'return expr;' must be compatible with the return type of the function
-- * The returned expression in 'return expr;' must be an l-value if the return intent is 'ref'
--
-- * 'break' and 'continue' cannot be used inside a For
-- * 'break' and 'contonue' cannot be used outside a loop
checkJmp :: Env -> Jump -> ET.ErrT Jump
checkJmp env@(c:cs) jmp = ET.toErrT jmp $ case jmp of
    Return loc    -> do
        when (inFor c || inWhile c) $ errorReturnLoop loc
        unless (returns c == TVoid) $ errorReturnProcedure loc $ returns c
        return jmp

    ReturnE loc r -> let t' = returns c in do
        (t, r') <- checkRExpError env r
        when (inFor c || inWhile c) $ errorReturnLoop loc
        unless (t `subtypeOf` t') $ errorReturnTypeMismatch loc t t'
        when ((isRef c) && (not (isLExp r))) $ errorReturnRef r
        return $ ReturnE loc r'

    Break loc     -> do
        when (inFor c) $ errorBreakFor loc
        unless (inWhile c) $ errorBreakOutside loc
        return jmp
    
    Continue loc  -> do
        when (inFor c) $ errorContinueFor loc
        unless (inWhile c) $ errorContinueOutside loc
        return jmp