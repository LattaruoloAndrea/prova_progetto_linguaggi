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
checkProgram env (Prog decls) = do
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
    
    FCall _ ident rs -> lookType ident env -- @TODO: check right numbers and types of rs

    PredR _ pread -> return $ tctypeOf pread

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
checkStm env stm = case stm of
    StmBlock block -> checkBlock env block

    StmCall ident rs -> do
        f <- lookFun ident env -- @TODO: check that arguments are ok with function definition
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
    FDecl id forms it ty blk -> do
        let params  = map formToParam forms             -- create Params from Forms
        let entries = map paramToEntry params           -- create EnvEntries from Params
        let env1    = pushFun env (tctypeOf ty) it
        unless (it==In || it==Ref) (EM.Bad "Return intent must be In or Ref.")
        env2 <- foldM makeEntry env1 $ zip [identFromParam p | p <- params] entries  -- fold the entry insertion given the list (id, entry)
        env3 <- checkBlock env2 blk
        return $ popContext env3

    CList cs -> do
        foldM checkCDecl env cs

    VList vs -> do
        foldM checkVDecl env vs


checkCDecl :: Env -> CDecl -> EM.Err Env
checkCDecl env c@(CDecl id t r) = do
    tr <- inferRExp env r                 -- Error purpose: check that r is well-typed
    unless (tr `subtypeOf` (tctypeOf t)) (EM.Bad $ "Error: type mismatch in a CONST DECLARATION at line " ++ (show $ locOf id) ++ ".")
    case constexpr env r of
        Nothing -> (EM.Bad "Error: the expression is not a const-expression in a CONST DECLARATION.")
        Just x  -> do

            makeConst env id x


checkVDecl :: Env -> VDecl -> EM.Err Env
checkVDecl env v = case v of
    Solo id t   -> makeMutable env id (tctypeOf t)

    Init id t r -> do
        let tc = tctypeOf t
        tr <- inferRExp env r
        unless (tr `subtypeOf` tc) $ badLoc (locOf id) "Type mismatch in a VAR DECLARATION"
        makeMutable env id tc


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
