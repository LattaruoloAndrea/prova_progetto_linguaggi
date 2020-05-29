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
typeCheck :: Program -> EM.Err Env
typeCheck = checkProgram startEnv


-- Loading of function names and check of all declarations
checkProgram :: Env -> Program -> EM.Err Env
checkProgram env (Prog decls) = ET.fromErrT $ do
    env1 <- foldM loadFunction env decls
    foldM checkDecl env1 decls


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
-- For a function, first we check for totality of non-procedures, then we check the rest
checkDecl :: Env -> Decl -> ET.ErrT Env
checkDecl env decl = case decl of
    FDecl id _ _ rt blk -> do
        unlessT env (((tctypeOf rt) == TVoid) || (checkReturnPaths $ stms blk)) $ errorReturnMissing id
        checkFDecl env decl

    CList cs -> foldM checkCDecl env cs

    VList vs -> foldM checkVDecl env vs


-- Check for the "totality" of a list of statements
-- (although the definition of totality for a statement is a bit stretched)
--
--   * a return statement is total
--   * an if/if-else is total if both his branches are total
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
            If _ s'                 -> checkReturnPaths [s']
            IfElse _ s1 s2          -> (checkReturnPaths [s1]) && (checkReturnPaths [s2])
            StmBlock b              -> checkReturnPaths $ stms b
            _                       -> False


-- Check for a function definition:
--   * return intent must be one of In (by value) or Ref (by reference)
--   * check that the body with the formal parameters added in is valid
checkFDecl :: Env -> Decl -> ET.ErrT Env
checkFDecl env (FDecl id forms it ty blk) =
    let params  = map formToParam forms             -- create Params from Forms
        entries = map paramToEntry params           -- create EnvEntries from Params
        env1    = pushFun env (tctypeOf ty) it
        makeEntry' = \e p -> ET.toErrT e $ makeEntry e p -- return the old env if something goes wrong
    in do
        unlessT () (it==In || it==Ref) $ errorReturnIntent (locOf id) it
        env2 <- foldM makeEntry' env1 $ zip [identFromParam p | p <- params] entries  -- fold the entry insertion given the list (id, entry)
        env3 <- checkBlock env2 blk
        return $ popContext env3


-- Extends the environment with a new compile-time constant (if possibile)
--  * If no duplicate declaration occures and type is correct and initializer is a compile-time constant, OkT (env++constant)
--  * Otherwise, BadT env
--
-- constexpr is defined in module CompileTime
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



-- Type inference for right expressions: all cases are delegated to minor functions
inferRExp :: Env -> RExp -> EM.Err TCType
inferRExp env rexp = case rexp of
    Or _ r1 r2          -> inferOr env r1 r2
    And _ r1 r2         -> inferAnd env r1 r2
    Not _ r             -> inferNot env r
    Comp _ r1 op r2     -> inferComp env r1 op r2
    Arith _ r1 op r2    -> inferArith env r1 op r2
    Sign _ _ r          -> inferRExp env r
    RefE _ l            -> inferRefE env l
    RLExp _ l           -> inferLExp env l
    ArrList loc rs      -> inferArrList env loc rs
    FCall _ ident rs    -> inferFCall env ident rs
    PredR _ pread       -> inferPredR env pread
    Lit _ literal       -> inferLiteral literal


-- Infer logical operators //////////////////////////////////////////////////////////

inferOr :: Env -> RExp -> RExp -> EM.Err TCType
inferOr = inferOrAnd errorOr

inferAnd :: Env -> RExp -> RExp -> EM.Err TCType
inferAnd = inferOrAnd errorAnd

inferNot :: Env -> RExp -> EM.Err TCType
inferNot env r = do
    inferLogicalOperand errorNot env r
    return TBool

-- Abstraction on the Or/And infer given a suitable error message handler
inferOrAnd :: (RExp -> TCType -> EM.Err a) -> Env -> RExp -> RExp -> EM.Err TCType
inferOrAnd errF env r1 r2 = do
    f r1
    f r2
    return TBool
    where
        errF' = \r t -> (errF r t) >> (return ())
        f = inferLogicalOperand errF' env

-- Abstraction on the single logical operand inference
inferLogicalOperand :: (RExp -> TCType -> EM.Err ()) -> Env -> RExp -> EM.Err Env
inferLogicalOperand errF env r = do
    t <- inferRExp env r
    unless (t == TBool) $ errF r t
    return env


-- Infer relational and arithmetic operators ///////////////////////////////////////////////////////////

inferComp :: Env -> RExp -> CompOp -> RExp -> EM.Err TCType
inferComp env r1 op r2 = do
    inferBinary env r1 op r2
    return TBool

inferArith :: Env -> RExp -> ArithOp -> RExp -> EM.Err TCType
inferArith = inferBinary

-- Abstraction over infer of relational and arithmetic operators
--   * Non compatible types of the operands throw an error
inferBinary :: (Show binop) => Env -> RExp -> binop -> RExp -> EM.Err TCType
inferBinary env r1 op r2 = do
    t1 <- inferRExp env r1
    t2 <- inferRExp env r2
    let t = supremum t1 t2
    when (t == TError) $ errorBinary op r1 r2 t1 t2
    return t


-- Minor RExp inferers /////////////////////////////////////////////////////////////////////

-- ArrayList type is the supremum of the inferred types inside the list
--   * Empty array throws an error
--   * Non compatible types throw an error
inferArrList :: Env -> Loc -> [RExp] -> EM.Err TCType
inferArrList env loc rs = case rs of
    [] -> errorArrayEmpty loc
    _  -> do
        t <- foldl1 supremumM $ map (inferRExp env) rs
        when (t == TError) $ errorArrayElementsCompatibility loc
        return $ TArr (length rs) t

-- Reference to a left-expression have type pointer to..
inferRefE :: Env -> LExp -> EM.Err TCType
inferRefE env l = do
    t <- inferLExp env l
    return $ TPoint t

-- Check of well-formed predefined reader call is delegated to checkCall
-- Type of a predefined read is monomorphic
inferPredR :: Env -> PRead -> EM.Err TCType
inferPredR env pread = do
    ET.fromErrT $ checkCall env (toIdent pread) []  -- toIdent is from AbsChapel
    return $ tctypeOf pread


-- Function calls have type of the return
--   * Check of well formed call is delegated to checkCall
inferFCall :: Env -> Ident -> [RExp] -> EM.Err TCType
inferFCall env ident rs = do
    ET.fromErrT $ checkCall env ident rs
    lookType ident env


-- Type of a literal is induced by the typeclass instanciation
inferLiteral :: Literal -> EM.Err TCType
inferLiteral literal = return $ tctypeOf literal



-- Infer of a left-expression: all cases are delegated to minor inferers
inferLExp :: Env -> LExp -> EM.Err TCType
inferLExp env lexp = case lexp of
    Deref l     -> inferDeref env l
    Access l r  -> inferAccess env l r
    Name ident  -> inferName env ident


-- Type of a dereference is induced by the type of the pointer
--   * If the left-expression is not a pointer, throw an error
inferDeref :: Env -> LExp -> EM.Err TCType
inferDeref env l = do
    t <- inferLExp env l
    case t of
        TPoint t' -> return t'
        _         -> errorNotAPointer l


-- Type of an array access is induced by the type of the array
--   * If the left-expression is not an array, throw an error
--   * If the right-expression is not subtype of an integer, throw an error
inferAccess :: Env -> LExp -> RExp -> EM.Err TCType
inferAccess env l r = do
    ta <- inferLExp env l
    ti <- inferRExp env r
    when (ti `supremum` TInt /= TInt) $ errorArrayIndex r
    case ta of
        TArr _ t -> return t
        _        -> errorArrayNot l


-- Type of a name is induced by it's entry in the environment
--   * If the name does not exist, Env.lookType will throw an error
inferName :: Env -> Ident -> EM.Err TCType
inferName env ident = lookType ident env



-- Infer right/left-expressions enhancing the error message:
-- this is used when we need to infer an expression in a check of a statement
-- 'checkExpError' is defined in module ErrorHandling
checkRExpError :: TCType -> Env -> RExp -> ET.ErrT TCType
checkRExpError = checkExpError inferRExp
    
checkLExpError :: TCType -> Env -> LExp -> ET.ErrT TCType
checkLExpError = checkExpError inferLExp



-- Check validity of a statement, given an environment: all cases are delegated to minor checkers
checkStm :: Env -> Stm -> ET.ErrT Env
checkStm env stm = case stm of
    StmBlock block -> checkBlock env block
    StmCall ident rs -> checkCall env ident rs
    PredW pwrite r -> checkCall env (toIdent pwrite) [r]
    Assign l _ r -> checkAssign env l r
    StmL l -> checkStmL env l
    If r stm -> checkIf env r stm
    IfElse r s1 s2 -> checkIfElse env r s1 s2
    While r s -> checkWhile env r s
    DoWhile s r -> checkDoWhile env r s
    For ident rng s -> checkFor env ident rng s
    JmpStm jmp -> checkJmpStm env jmp


-- Empty block
emptyBlock :: Block
emptyBlock = Block (Loc 0 0) [] []

-- Check of a block statement:
-- * Push a new empty context
-- * Load all function names (to allow mutual-recursion)
-- * Check all declarations
-- * Check all statements
-- * Pop the new context
--
-- @returns the environment changed adding the subjects of the declaration
checkBlock :: Env -> Block -> ET.ErrT Env
checkBlock env block = do
    let env1 = pushContext env
    env2 <- foldM loadFunction env1 (decls block)
    env3 <- foldM checkDecl env2 (decls block)
    env4 <- foldM checkStm env3 (stms block)
    return $ popContext env4


-- Helper for check of a function call:
-- check that the expression passed is compatible with the parameter type and intent
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


-- Check of a function call:
--   * Function must exist in the scope, otherwise Env.lookFun will throw an error
--   * If actual and formal parameters lists have not the same length, throw an error
--   * If actual and formal types and intents are not compatible, throw an error
checkCall :: Env -> Ident -> [RExp] -> ET.ErrT Env
checkCall env ident actuals = ET.toErrT env $ do
    f <- lookFun ident env
    let la = length actuals
        params = paramsOf f
        lp = length params
    unless (la == lp) $ errorCallWrongNumber ident la lp
    ET.fromErrT $ foldM checkPassing env $ zip actuals params
    return env


-- Check an assignment statement
--   * If the left-expression is ill-formed, throw an error
--   * If the right-expression is ill-formed, throw an error
--   * If not (leftType >= rightType), throw an error
--   * If left-expression is immutable, throw an error
checkAssign :: Env -> LExp -> RExp -> ET.ErrT Env
checkAssign env l r = do
    tl <- checkLExpError TError env l
    tr <- checkRExpError tl env r
    unlessT env (tr `subtypeOf` tl) $ errorAssignType $ locOf l
    unlessT env (isMutable env l)   $ errorAssignImmutable l
    -- whenT   env (isFunction env l)  $ errorAssignFunction $ locOf l
    return env

-- Check a left-expression statement
--   * If the expression is ill-formed, throw an error
checkStmL :: Env -> LExp -> ET.ErrT Env
checkStmL env l = do
    checkLExpError TVoid env l
    return env


-- Check of if-then statement is delegated to if-then-else with an empty else-block
checkIf :: Env -> RExp -> Stm -> ET.ErrT Env
checkIf env r s = checkIfElse env r s (StmBlock emptyBlock)

-- Check of an if-then-else statement:
--   * If the guard-expression is ill-formed, throw an error
--   * If the guard-expression is not a bool, throw an error
checkIfElse :: Env -> RExp -> Stm -> Stm -> ET.ErrT Env
checkIfElse env r s1 s2 = do
    tr <- checkRExpError TBool env r
    unlessT env (tr == TBool) $ errorGuard r tr
    let thenEnv = pushContext env                   -- Entering the scope of then
    thenEnv' <- checkStm thenEnv s1
    let elseEnv = pushContext $ popContext thenEnv' -- Exiting the scope of then and entering the scope of else
    elseEnv' <- checkStm elseEnv s2
    return $ popContext elseEnv'                    -- Exiting the scope of else and return the new env


-- Check of a while-statement:
--   * If the guard-expression is ill-formed, throw an error
--   * If the guard-expression is not a bool, throw an error
checkWhile :: Env -> RExp -> Stm -> ET.ErrT Env
checkWhile env r s = do
    tr <- checkRExpError TBool env r
    unlessT env (tr == TBool) $ errorGuard r tr
    let loopEnv = pushWhile env                     -- Entering the loop scope
    exitEnv <- checkStm loopEnv s
    return $ popContext exitEnv                     -- Exiting the loop scope


-- Do-while is delegated to checkWhile 
checkDoWhile :: Env -> RExp -> Stm -> ET.ErrT Env
checkDoWhile = checkWhile


-- check of for-statement:
--   * If range is ill-formed, throw an error
checkFor :: Env -> Ident -> Range -> Stm -> ET.ErrT Env
checkFor env ident rng s = do
    checkRange env rng
    let loopEnv  = pushFor env
    loopEnv' <- ET.toErrT loopEnv $ makeForCounter loopEnv ident
    exitEnv <- checkStm loopEnv' s
    return $ popContext exitEnv


-- check of a for-range:
--   * If start/end expressions are ill-formed, throw an error
--   * If start/end expression are not a subtype of integer, throw an error
checkRange :: Env -> Range -> ET.ErrT Env
checkRange env rng = do
    ts <- checkRExpError TInt env $ start rng
    te <- checkRExpError TInt env $ end rng
    unlessT env (ts `subtypeOf` TInt) $ errorRangeStart (locOf rng) (start rng) ts
    unlessT env (te `subtypeOf` TInt) $ errorRangeEnd (locOf rng) (end rng) te
    return env


-- Check of a jump statement is delegated
checkJmpStm :: Env -> Jump -> ET.ErrT Env
checkJmpStm env jmp = do
    checkJmp env jmp
    return env

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
checkJmp :: Env -> Jump -> ET.ErrT ()
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