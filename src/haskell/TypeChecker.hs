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


-- Default basic literals
defBool     = LBool False
defChar     = LChar '\NUL'
defInt      = LInt 0
defReal     = LReal 0.0
defString   = LString ""

-- Get a default literal of some type
getDefault :: TCType -> Literal
getDefault t = case t of
    TBool       -> defBool
    TChar       -> defChar
    TInt        -> defInt
    TReal       -> defReal
    TString     -> defString
    TPoint _    -> LNull
    TArr c d t' -> LArr c $ map getDefault $ replicate d t'

predLoc :: Loc
predLoc = Loc (-1) (-1)

-- List of predefined functions
predList :: [(String, Entry)]
predList = [
    ("readChar",    Fun predLoc [] In TChar),
    ("readInt",     Fun predLoc [] In TInt),
    ("readReal",    Fun predLoc [] In TReal),
    ("readString",  Fun predLoc [] In TString),
    ("writeChar",   Fun predLoc [Param predLoc "x" In TChar] In TVoid),
    ("writeInt",    Fun predLoc [Param predLoc "x" In TInt] In TVoid),
    ("writeReal",   Fun predLoc [Param predLoc "x" In TReal] In TVoid),
    ("writeString", Fun predLoc [Param predLoc "x" In TString] In TVoid),
    ("stringCmp",   Fun predLoc [Param predLoc "x" ConstRef TString, Param predLoc "op" In TInt, Param predLoc "y" ConstRef TString] In TBool)]

-- Initial environment (with the global context)
startEnv :: Env
startEnv = [Context (M.fromList predList) TVoid False False False]

-- Typecheck starting point: check of a program with the startEnv
typeCheck :: Program () -> EM.Err (Program TCType)
typeCheck = checkProgram startEnv


-- Function that abstracts a process needed to foldM other functions (checkDecl, checkCDecl, checkVDecl, etc...)
foldWrapper :: (Env -> a -> ET.ErrT (Env, b)) -> ((Env, [b]) -> a -> ET.ErrT (Env, [b]))
foldWrapper f = 
    \(e, ls) a -> do
        (e', b) <- f e a
        return (e', b : ls)

-- foldWrapper for checkStm
foldWrapper' :: (Env -> a -> ET.ErrT b) -> ((Env, [b]) -> a -> ET.ErrT (Env, [b]))
foldWrapper' f =
    \(e, ls) a -> do
        b <- f e a
        return (e, b : ls)


-- Loading of function names and check of all declarations
checkProgram :: Env -> Program () -> EM.Err (Program TCType)
checkProgram env (Prog decls) = ET.fromErrT $ do
    unlessT () ( hasMain ) $ errorMissingMain
    env1 <- foldM loadFunction env decls
    eds <- foldM (foldWrapper checkDecl) (env1, []) decls
    return . Prog . reverse $ snd eds       -- decls were consed in reverse
    where
        hasMain = any helper decls
        helper (FDecl id ps _ rt _) = idName id == "main" && null ps && tctypeOf rt == TVoid
        helper _ = False


-- Insert function name in the scope (to be used once we enter in a new block to permit mutual-recursion)
--  * If no duplicate declaration occures, OkT (env++function)
--  * Otherwise, BadT env
loadFunction :: Env -> Decl () -> ET.ErrT Env
loadFunction env d = ET.toErrT env $ case d of
    FDecl id forms it ty _  ->
        let params = map formToParam forms      -- formToParam is in Env.hs
        in makeFun env id params it $ tctypeOf ty
    
    _                       -> return env


-- Check of a declaration: for variables and compile-time constants lists just fold of an individual check
-- For a function, first we filter-out unreachable statements and then we check the "filtered" function
checkDecl :: Env -> Decl () -> ET.ErrT (Env, Decl TCType)
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
reachables :: [Stm ()] -> [Stm ()]
reachables ss = ss'
    where
        extends = \s -> case s of   -- Transform a block in a block of reachable statements
            StmBlock b  -> StmBlock $ Block (locOf b) (decls b) (reachables $ stms b)
            _           -> s
        
        isSink = \s -> case s of    -- Predicate to capture the notion of sink
            JmpStm (Return _)       -> True
            JmpStm (ReturnE _ _ _)  -> True
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
checkReturnPaths :: [Stm ()] -> Bool
checkReturnPaths ls = foldr helper False ls
    where
        helper = \s acc -> acc || case s of
            JmpStm (Return _)       -> True
            JmpStm (ReturnE _ _ _)  -> True
            IfElse _ s1 s2          -> (checkReturnPaths [s1]) && (checkReturnPaths [s2])
            StmBlock b              -> checkReturnPaths $ stms b
            _                       -> False


-- Check that parameters passed by res/valres are assigned to a value in all execution paths
checkRes :: Env -> Ident -> [Param] -> [Stm ()] -> ET.ErrT ()
checkRes env id params ss = foldl (>>) (return ()) errs
    where
        errs    = map ferr params
        ferr    = \x@(Param _ n _ _) ->     -- Given a Param x, set error when at the end there is an exec path where it doesn't have a value assigned
                    if helper x ss
                    then return ()
                    else ET.toErrT () $ errorMissingAssignRes (locOf x) (idName id) n
        helper  = \x@(Param _ n _ _) ss ->
            let helper' = \s acc -> case s of   -- Roughly the same structure of return-paths
                    JmpStm (Return _)                       -> False
                    JmpStm (ReturnE _ _ _)                  -> False
                    Assign (Name ident _) _ _               -> acc || (idName ident) == n
                    Assign (Access (Name ident _) _ _) _ _  -> acc || (idName ident) == n
                    IfElse _ s1 s2                          -> acc || (helper x [s1]) && (helper x [s2])
                    StmBlock b                              -> acc || (helper x $ stms b)
                    _                                       -> acc
            in foldr helper' False ss

-- Check for a function definition:
--   * return intent must be either In (by value) or Ref (by reference)
--   * Proper functions must reach a 'return' in every possibile exec path
--   * check that all res/valres parameters will reach an assignment in every exec path
--   * check that the body with the formal parameters added in is valid
checkFDecl :: Env -> Decl () -> ET.ErrT (Env, Decl TCType)
checkFDecl env f@(FDecl id forms it ty blk) = ET.toErrT (env, toTCT f) $ do
    let helperForm (Form it id t) = do
            t' <- checkType env t
            when (tctypeOf t' == TVoid) $ errorParameterVoid id
            return (Form it id t')
    forms' <- mapM helperForm forms
    ty' <- checkType env ty
    ET.fromErrT $ extra forms' ty'
    where
        extra forms' ty' = 
            let params          = map formToParam forms             -- create Params from Forms
                resCandidates   = filter (\(Param _ _ it _) -> it == Out || it == InOut) params -- Get params passed by res/valres
                entries         = map paramToEntry params           -- create EnvEntries from Params
                env1            = pushFun env (tctypeOf ty) it
                makeEntry' = \e p -> ET.toErrT e $ makeEntry e p -- return the old env if something goes wrong
            in do
                unlessT () (it==In || it==Ref) $ errorReturnIntent (locOf id) it
                whenT   () (tctypeOf ty /= TVoid && (not $ checkReturnPaths $ stms blk)) $ errorReturnMissing id
                checkRes env1 id resCandidates $ stms blk
                env2 <- foldM makeEntry' env1 $ zip [identFromParam p | p <- params] entries  -- fold the entry insertion given the list (id, entry)
                (env3, blk') <- checkScope env2 blk
                return (popContext env3, FDecl id forms' it ty' blk')


-- Extends the environment with a new compile-time constant (if possibile)
--  * If no duplicate declaration occures and type is correct and initializer is a compile-time constant, OkT (env++constant)
--  * Otherwise, BadT env
--
-- constexpr is defined in module CompileTime
checkCDecl :: Env -> CDecl () -> ET.ErrT (Env, CDecl TCType)
checkCDecl env c@(CDecl id t r) = do
    pp <- checkTypeCase env t r (toTCT c) f
    whenT pp (tctypeOf t == TVoid) $ errorConstantVoid id
    where
        f t' r = ET.toErrT (env, toTCT c) $ let tc = tctypeOf t' in do
            r' <- inferRExp env r                 -- Error purpose: check that r is semantically correct
            let tr = tctypeOf r'
            unless (tr `subtypeOf` tc) $ errorConstTypeMismatch id tc tr
            case constexpr env r of
                Nothing -> errorNotConst id r
                Just x  -> do
                    env' <- makeConst env id x
                    return (env', CDecl id t' $ coerce tc (Lit (locOf r') x tr))


-- Extends the environment with a new variable (if possible)
--  * If no duplicate declaration occures and type of initialization is correct, OkT (env++initialized) 
--  * If no duplicate declaration occures and type of initialization is incorrect, BadT (env++not_initialized)
--  * If duplicate declaration occures, BadT env
checkVDecl :: Env -> VDecl () -> ET.ErrT (Env, VDecl TCType)
checkVDecl env v = case v of
    Solo id t   -> do
        pp <- checkTypeCase env t () (toTCT v) f
        whenT pp (tctypeOf t == TVoid) $ errorVariableVoid id
        where
            f t' _ = do
                let tv = tctypeOf t'
                env' <- ET.toErrT env $ makeMutable env id tv
                return (env', Init id t' $ Lit (locOf id) (getDefault tv) tv)

    Init id t r -> do
        pp <- checkTypeCase env t r (toTCT v) g
        whenT pp (tctypeOf t == TVoid) $ errorVariableVoid id
        where
            g t' r = do
                let tc = tctypeOf t' 
                    mk = makeMutable env id tc
                case mk of
                    EM.Ok env'  -> ET.toErrT (env', Solo id t') $ do
                        r' <- inferRExp env r
                        let tr = tctypeOf r'
                        unless (tr `subtypeOf` tc) $ errorVarTypeMismatch id tc tr
                        return (env', Init id t' (coerce tc r'))
                    EM.Bad s    -> ET.badT (env, toTCT v) s




checkTypeCase :: Env -> Type () -> a -> b -> (Type TCType -> a -> ET.ErrT (Env, b)) -> ET.ErrT (Env, b)
checkTypeCase env t a ifBad ifOk = case checkType env t of
    EM.Bad s -> ET.badT (env, ifBad) s
    EM.Ok t' -> ifOk t' a



checkType :: Env -> Type () -> EM.Err (Type TCType)
checkType env (Type c b) = do
    c' <- helper env c
    return $ Type c' b
    where
        helper env c = case c of
            Simple          -> return Simple
            Pointer c'      -> do
                c'' <- helper env c'
                return $ Pointer c''
            Array ch c' r   -> do
                c'' <- helper env c'
                r' <- inferRExp env r
                let tr = tctypeOf r'
                unless (tr `subtypeOf` TInt) $ errorArrayNonIntegerSize r tr
                case constexpr env r' of
                    Just x  -> return $ Array ch c'' $ Lit (locOf r) (toLInt x) TInt
                    Nothing -> errorArrayNonConstantSize r



-- Type t inference for right expressions: all cases are delegated to minor functions
inferRExp :: Env -> RExp () -> EM.Err (RExp TCType)
inferRExp env rexp = (case rexp of
    Or _ _ _ _      -> inferOr
    And _ _ _ _     -> inferAnd
    Not _ _ _       -> inferNot
    Comp _ _ _ _ _  -> inferComp
    Arith _ _ _ _ _ -> inferArith
    Sign _ _ _ _    -> inferSign
    RefE _ _ _      -> inferRefE
    RLExp _ _ _     -> inferRLExp
    ArrList _ _ _   -> inferArrList
    FCall _ _ _ _ _ -> inferFCall
    Lit _ _ _       -> inferLit)
    env rexp


-- Infer logical operators //////////////////////////////////////////////////////////

inferOr :: Env -> RExp () -> EM.Err (RExp TCType)
inferOr env (Or l r1 r2 _) = do
    r1' <- inferLogicalOperand errorOr env r1
    r2' <- inferLogicalOperand errorOr env r2
    return $ Or l r1' r2' TBool

inferAnd :: Env -> RExp () -> EM.Err (RExp TCType)
inferAnd env (And l r1 r2 _) = do
    r1' <- inferLogicalOperand errorAnd env r1
    r2' <- inferLogicalOperand errorAnd env r2
    return $ And l r1' r2' TBool

inferNot :: Env -> RExp () -> EM.Err (RExp TCType)
inferNot env (Not l r _) = do
    r' <- inferLogicalOperand errorNot env r
    return $ Not l r' TBool

-- Abstraction on the single logical operand inference
inferLogicalOperand :: (RExp () -> TCType -> EM.Err ()) -> Env -> RExp () -> EM.Err (RExp TCType)
inferLogicalOperand errF env r = do
    r' <- inferRExp env r
    let t = tctypeOf r'
    unless (t == TBool) $ errF r t
    return r'


-- Infer relational and arithmetic operators ///////////////////////////////////////////////////////////


-- For relational operators we need both operands to be compatible with each other
inferComp :: Env -> RExp () -> EM.Err (RExp TCType)
inferComp env (Comp loc r1 op r2 _) = do
    r1' <- inferRExp env r1
    r2' <- inferRExp env r2
    let t1 = tctypeOf r1'
        t2 = tctypeOf r2'
        t = supremum t1 t2
    when (t == TError) $ errorBinary op r1 r2 t1 t2
    case t of
        TString  -> return $ FCall loc (Ident predLoc "stringCmp") [coerce t r1', opToNum, coerce t r2'] [ConstRef, In, ConstRef] TBool
        _        -> return $ Comp loc (coerce t r1') (set t op) (coerce t r2') TBool
    where
        opToNum = Lit loc (LInt n) TInt
        n = case op of
            Lt  _ -> 0
            Leq _ -> 1
            Eq  _ -> 2
            Neq _ -> 3
            Geq _ -> 4
            Gt  _ -> 5
            

-- For arithmetic operators we need "numeric" operands (i.e. subtype of real)
-- extra conditions for '%' (operands both subtype of int) and '^' (exponent subtype of int)
inferArith :: Env -> RExp () -> EM.Err (RExp TCType)
inferArith env (Arith loc r1 op r2 _) = do
    r1' <- inferRExp env r1
    r2' <- inferRExp env r2
    let t1 = tctypeOf r1'
        t2 = tctypeOf r2'
        t = supremum t1 t2
    when (t == TError) $ errorBinary op r1 r2 t1 t2
    unless (t `subtypeOf` TReal) $ errorArithmetic op r1 r2 t1 t2
    case op of
        Mod   ->  (unless (t1 `subtypeOf` TInt) $ errorArithOperandInt r1 t1) >>
                  (unless (t2 `subtypeOf` TInt) $ errorArithOperandInt r2 t2)
        Pow _ ->   unless (t2 `subtypeOf` TInt) $ errorArithOperandInt r2 t2
        _     ->  return ()
    let t' = supremum t TInt
    return $ Arith loc (coerce t' r1') (set t' op) (coerce t' r2') t'


inferSign :: Env -> RExp () -> EM.Err (RExp TCType)
inferSign env rexp@(Sign loc op r _) = do
    r' <- inferRExp env r
    let t = tctypeOf r' `supremum` TInt
    unless (t `subtypeOf` TReal) $ errorSignNotNumber rexp t
    return $ Sign loc (set t op) (coerce t r') t

-- ArrayList type is the supremum of the inferred types inside the list
--   * Empty array throws an error
--   * Non compatible types throw an error
inferArrList :: Env -> RExp () -> EM.Err (RExp TCType)
inferArrList env (ArrList loc rs _) = case rs of
    [] -> errorArrayEmpty loc
    _  -> do
        rs' <- mapM (inferRExp env) rs
        let t = foldl1 supremum $ map tctypeOf rs'
        when (t == TError) $ errorArrayElementsCompatibility loc
        let tt = TArr False (length rs) t
            cs = map (coerce t) rs'
        return $ ArrList loc cs tt

-- Reference to a left-expression have type pointer to..
inferRefE :: Env -> RExp () -> EM.Err (RExp TCType)
inferRefE env (RefE loc l _) = do
    unless (isMutable env l) $ errorRefToImmutable l
    l' <- inferLExp env l
    let t = tctypeOf l'
    return $ RefE loc l' (TPoint t)

-- Right-left expressions have type of the left expression
inferRLExp :: Env -> RExp () -> EM.Err (RExp TCType)
inferRLExp env (RLExp loc l _) = do
    l' <- inferLExp env l
    let tl = tctypeOf l'
    return $ RLExp loc l' tl


-- Function calls have type of the return
--   * Check of well formed call is delegated to checkCall
inferFCall :: Env -> RExp () -> EM.Err (RExp TCType)
inferFCall env call@(FCall loc id@(Ident l n) rs _ _) = do
    let cc = checkCall env id rs
    (rs', its, l') <- ET.fromErrT cc
    rt  <- lookType id env
    when (rt == TVoid) $ errorCallNotAFunction call
    return $ FCall loc (Ident l' n) rs' its rt


-- Type t of a literal is induced by the typeclass instance
inferLit :: Env -> RExp () -> EM.Err (RExp TCType)
inferLit _ (Lit loc l _) = return $ Lit loc l (tctypeOf l)



-- Infer of a left-expression: all cases are delegated to minor inferers
inferLExp :: Env -> LExp () -> EM.Err (LExp TCType)
inferLExp env lexp = case lexp of
    Deref  _ _   -> inferDeref env lexp
    Access _ _ _ -> inferAccess env lexp
    Name   _ _   -> inferName env lexp


-- Type t of a dereference is induced by the type of the pointer
--   * If the left-expression is not a pointer, throw an error
inferDeref :: Env -> LExp () -> EM.Err (LExp TCType)
inferDeref env (Deref l _) = do
    l' <- inferLExp env l
    case tctypeOf l' of
        TPoint t' -> return $ Deref l' t'
        _         -> errorNotAPointer l


-- Type t of an array access is induced by the type of the array
--   * If the left-expression is not an array, throw an error
--   * If the right-expression is not subtype of an integer, throw an error
inferAccess :: Env -> LExp () -> EM.Err (LExp TCType)
inferAccess env (Access l r _) = do
    l' <- inferLExp env l
    r' <- inferRExp env r
    let ti = tctypeOf r'
        ta = tctypeOf l'
    when (ti `supremum` TInt /= TInt) $ errorArrayIndex r
    case ta of
        TArr  _ _ t -> return $ Access l' r' t
        _           -> errorArrayNot l


-- Type t of a name is induced by it's entry in the environment
--   * If the name does not exist, Env.lookType will throw an error
inferName :: Env -> LExp () -> EM.Err (LExp TCType)
inferName env (Name id@(Ident l n) _) = do
    entry <- lookName id env
    return $ Name (Ident (locOf entry) n) (tctypeOf entry)



-- Infer right/left-expressions enhancing the error message:
-- this is used when we need to infer an expression in a check of a statement
-- 'checkExpError' is defined in module ErrorHandling
checkRExpError :: Env -> RExp () -> EM.Err (RExp TCType)
checkRExpError = checkExpError inferRExp
    
checkLExpError :: Env -> LExp () -> EM.Err (LExp TCType)
checkLExpError = checkExpError inferLExp



-- Check validity of a statement, given an environment: all cases are delegated to minor checkers
checkStm :: Env -> Stm () -> ET.ErrT (Stm TCType)
checkStm env stm = case stm of
    StmBlock _      -> checkBlock env stm
    StmCall  _ _ _  -> checkStmCall env stm
    Assign   _ _ _  -> checkAssign env stm
    StmL     _      -> checkStmL env stm
    If       _ _    -> checkIf env stm
    IfElse   _ _ _  -> checkIfElse env stm
    While    _ _    -> checkWhile env stm
    DoWhile  _ _    -> checkDoWhile env stm
    For      _ _ _  -> checkFor env stm
    JmpStm   _      -> checkJmpStm env stm


-- Empty block
emptyBlock :: Block t
emptyBlock = Block (Loc 0 0) [] []

-- Check of a block statement:
-- * Push a new empty context
-- * Check the new scope created
-- * Pop the new context
checkBlock :: Env -> Stm () -> ET.ErrT (Stm TCType)
checkBlock env (StmBlock block) = do
    let env1 = pushContext env
    (env2, block') <- checkScope env1 block
    return $ StmBlock block'

-- Check of a scope:
-- * Load all function names (to allow mutual recursion)
-- * Add al new declared variables/constants
-- * Check the list of statements
checkScope :: Env -> Block () -> ET.ErrT (Env, Block TCType)
checkScope env block = do
    env1 <- foldM loadFunction env (decls block)
    (env2, ds) <- foldM (foldWrapper checkDecl) (env1, []) (decls block)
    (env3, ss) <- foldM (foldWrapper' checkStm) (env2, []) (stms block)
    return (env, Block (locOf block) (reverse ds) (reverse ss))


-- Helper for check of a function call:
-- check that the expression passed is compatible with the parameter type and intent
checkPassing :: Env -> (RExp TCType, Param) -> ET.ErrT Env
checkPassing env (r, Param loc id it tp) =
    let t = tctypeOf r in case it of
        In          ->  unlessT env (t `subtypeOf` tp) $ errorPassingTypeSub r t tp
        
        Out         -> do
            unlessT env (tp `subtypeOf` t) $ errorPassingTypeSuper r t tp 
            unlessT env (isLExp r) $ errorPassingLExp r
            caseImmutable
        
        InOut       -> do
            unlessT env (t == tp) $ errorPassingTypeSame r t tp
            unlessT env (isLExp r) $ errorPassingLExp r
            caseImmutable
            
        Ref         -> do
            unlessT env (t == tp) $ errorPassingTypeSame r t tp
            unlessT env (isLExp r) $ errorPassingLExp r
            caseImmutable

        ConstIn     ->  unlessT env (t `subtypeOf` tp)  $ errorPassingTypeSub r t tp
        
        ConstRef    -> do
            unlessT env (t == tp) $ errorPassingTypeSame r t tp
            unlessT env (isLExp r) $ errorPassingLExp r
    where
        caseImmutable = case r of
            (RLExp _ l _) -> unlessT env (isMutable env l) $ errorPassingImmutable it r
            _             -> return env

-- Check of a function call:
--   * Function must exist in the scope, otherwise Env.lookFun will throw an error
--   * If actual and formal parameters lists have not the same length, throw an error
--   * If actual and formal types and intents are not compatible, throw an error
checkCall :: Env -> Ident -> [RExp ()] -> ET.ErrT ([RExp TCType], [Intent], Loc)
checkCall env ident actuals =
    let actuals' = map toTCT actuals
    in  ET.toErrT (actuals', [], locOf ident) $ do
        actuals' <- mapM (inferRExp env) actuals
        f <- lookFun ident env
        let la = length actuals'
            params = paramsOf f
            lp = length params
        unless (la == lp) $ errorCallWrongNumber ident la lp
        ET.fromErrT $ foldM checkPassing env $ zip actuals' params
        return (actuals', intentsOf params, locOf f)

checkStmCall :: Env -> Stm () -> ET.ErrT (Stm TCType)
checkStmCall env (StmCall id@(Ident l n) actuals _) = do
    (rs, its, l') <- checkCall env id actuals
    rt <- ET.toErrT TVoid $ lookType id env
    unlessT () (rt == TVoid) $ errorCallNotAProcedure id rt
    return $ StmCall (Ident l' n) rs its


-- Check an assignment statement
--   * Function-names cannot be on the left of an assignment
--   * If the left-expression is ill-formed, throw an error
--   * If the right-expression is ill-formed, throw an error
--   * If not (leftType >= rightType), throw an error
--   * If left-expression is immutable, throw an error
--   * Special check for '%=' and '^='
--   * Right-expression must be a 'number' (subtype of real) when non-eq assignments are used
checkAssign :: Env -> Stm () -> ET.ErrT (Stm TCType)
checkAssign env stm@(Assign l op r) =
    let stm' = toTCT stm
    in  ET.toErrT stm' $ do
        when (isFunction env l)  $ errorAssignFunction $ locOf l
        unless (isMutable env l)   $ errorAssignImmutable l
        l' <- checkLExpError env l
        r' <- checkRExpError env r
        let tl = tctypeOf l'
            tr = tctypeOf r'
        unless (tr `subtypeOf` tl) $ errorAssignType l' r'
        case op of
            AssignMod _   -> do
                unless (tl `subtypeOf` TInt) $ errorAssignMod l tl
                let tl' = TInt
                    tr' = TInt
                    top = TInt
                return $ Assign l' (set top op) (coerce tr' r')
            AssignPow _ _ -> do
                unless (tr `subtypeOf` TInt) $ errorAssignPow r tr
                let tl' = TInt `supremum` tl
                    tr' = TInt
                    top = tl'
                return $ Assign l' (set top op) (coerce tr' r')
            AssignEq  _ _ -> do
                let tl' = tl
                    tr' = tl
                    top = tl
                return $ Assign l' (set top op) (coerce tr' r')
            _             -> do
                unless (tr `subtypeOf` TReal) $ errorAssignNotReal r tr
                let tl' = tl
                    tr' = tl
                    top = tl
                return $ Assign l' (set top op) (coerce tr' r')

-- Check a left-expression statement
--   * If the expression is ill-formed, throw an error
checkStmL :: Env -> Stm () -> ET.ErrT (Stm TCType)
checkStmL env stm@(StmL l) =
    let stm' = toTCT stm
    in ET.toErrT stm' $ do
        l' <- checkLExpError env l
        return $ StmL l'


-- Check of if-then statement is delegated to if-then-else with an empty else-block
checkIf :: Env -> Stm () -> ET.ErrT (Stm TCType)
checkIf env (If r s) = do
    (r', s1', s2') <- ifHelper env r s (StmBlock emptyBlock)
    return $ If r' s1'

-- Check of an if-then-else statement:
--   * If the guard-expression is ill-formed, throw an error
--   * If the guard-expression is not a bool, throw an error
checkIfElse :: Env -> Stm () -> ET.ErrT (Stm TCType)
checkIfElse env (IfElse r s1 s2) = do
    (r', s1', s2') <- ifHelper env r s1 s2
    return $ IfElse r' s1' s2'

ifHelper :: Env -> RExp () -> Stm () -> Stm () -> ET.ErrT (RExp TCType, Stm TCType, Stm TCType)
ifHelper env r s1 s2 = do
    r' <- ET.toErrT (set TBool r) $ checkRExpError env r
    let tr = tctypeOf r'
    unlessT () (tr == TBool) $ errorGuard r tr
    let thenEnv = pushContext env                   -- Entering the scope of then
    s1' <- checkStm thenEnv s1
    let elseEnv = pushContext $ popContext thenEnv -- Exiting the scope of then and entering the scope of else
    s2' <- checkStm elseEnv s2
    return (r', s1', s2')


-- Check of a while-statement:
--   * If the guard-expression is ill-formed, throw an error
--   * If the guard-expression is not a bool, throw an error
checkWhile :: Env -> Stm () -> ET.ErrT (Stm TCType)
checkWhile env (While r s) = do
    (r', s') <- loopHelper env r s
    return $ While r' s'                     -- Exiting the loop scope


-- Do-while is delegated to checkWhile 
checkDoWhile :: Env -> Stm () -> ET.ErrT (Stm TCType)
checkDoWhile env (DoWhile s r) = do
    (r', s') <- loopHelper env r s
    return $ DoWhile s' r'

loopHelper :: Env -> RExp () -> Stm () -> ET.ErrT (RExp TCType, Stm TCType)
loopHelper env r s = do
    r' <- ET.toErrT (set TBool r) $ checkRExpError env r
    let tr = tctypeOf r'
    unlessT () (tr == TBool) $ errorGuard r tr
    let loopEnv = pushWhile env                     -- Entering the loop scope
    s' <- checkStm loopEnv s
    return (r', s')


-- check of for-statement:
--   * If range is ill-formed, throw an error
checkFor :: Env -> Stm () -> ET.ErrT (Stm TCType)
checkFor env (For ident rng s) = do
    rng' <- checkRange env rng
    let loopEnv  = pushFor env
    loopEnv' <- ET.toErrT loopEnv $ makeForCounter loopEnv ident
    s' <- checkStm loopEnv' s
    return $ For ident rng' s'


-- check of a for-range:
--   * If start/end expressions are ill-formed, throw an error
--   * If start/end expression are not a subtype of integer, throw an error
checkRange :: Env -> Range () -> ET.ErrT (Range TCType)
checkRange env (Range loc st en) = do
    st' <- ET.toErrT (set TInt st) $ checkRExpError env st
    en' <- ET.toErrT (set TInt st) $ checkRExpError env en
    let ts = tctypeOf st'
        te = tctypeOf en'
    unlessT () (ts `subtypeOf` TInt) $ errorRangeStart loc st ts
    unlessT () (te `subtypeOf` TInt) $ errorRangeEnd loc en te
    return $ Range loc (coerce TInt st') (coerce TInt en')


-- Check of a jump statement is delegated
checkJmpStm :: Env -> Stm () -> ET.ErrT (Stm TCType)
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
checkJmp :: Env -> Jump () -> ET.ErrT (Jump TCType)
checkJmp env@(c:cs) jmp = ET.toErrT (toTCT jmp) $ case jmp of
    Return loc    -> do
        when (inFor c || inWhile c) $ errorReturnLoop loc
        unless (returns c == TVoid) $ errorReturnProcedure loc $ returns c
        return $ toTCT jmp

    ReturnE loc r _ -> let t' = returns c in do
        r' <- checkRExpError env r
        let t = tctypeOf r'
        when (inFor c || inWhile c) $ errorReturnLoop loc
        unless (t `subtypeOf` t') $ errorReturnTypeMismatch loc t t'
        when ((isRef c) && (not (isLExp r))) $ errorReturnRef r
        return $ ReturnE loc (coerce t' r') t'

    Break loc     -> do
        when (inFor c) $ errorBreakFor loc
        unless (inWhile c) $ errorBreakOutside loc
        return $ toTCT jmp
    
    Continue loc  -> do
        when (inFor c) $ errorContinueFor loc
        unless (inWhile c) $ errorContinueOutside loc
        return $ toTCT jmp