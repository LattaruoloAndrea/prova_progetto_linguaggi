module TypeChecker where

import AbsChapel
import TCType
import TCInstances
import Locatable
import Env
import Control.Monad (unless, when)
import qualified ErrM as EM

import Control.Monad (when)

-- Copied from Skel --------------------------------------
type Result = EM.Err TCType

failure :: Show a => a -> Result
failure x = EM.Bad $ "Undefined case: " ++ show x
----------------------------------------------------------


inferRExp :: Env -> RExp -> EM.Err TCType
inferRExp env rexp = case rexp of -- Let to be deleted when all cases are completed
    Or _ rexp1 rexp2 -> do
        t1 <- inferRExp env rexp1
        t2 <- inferRExp env rexp2
        unless (t1 == TBool && t2 == TBool) (EM.Bad "Operands are not of type bool in a OR expression.")
        return TBool

    And _ rexp1 rexp2 -> do
        t1 <- inferRExp env rexp1
        t2 <- inferRExp env rexp2
        unless (t1 == TBool && t2 == TBool) (EM.Bad "Operands are not of type bool in a AND expression.")
        return TBool
    
    Not _ rexp -> do
        t <- inferRExp env rexp
        unless (t == TBool) (EM.Bad "Operand is not of type bool in a NOT expression.")
        return TBool
        
    Comp _ rexp1 compop rexp2 -> do
        t1 <- inferRExp env rexp1
        t2 <- inferRExp env rexp2
        let t = supremum t1 t2
        when (t == TError) (EM.Bad "Operands are not compatible in a COMPARISON expression.")
        return TBool

    Arith _ rexp1 arithop rexp2 -> do
        t1 <- inferRExp env rexp1
        t2 <- inferRExp env rexp2
        let t = supremum t1 t2
        when (t == TError) (EM.Bad "Operands are not compatible in an ARITHMETIC expression.")
        return t

    Sign _ signop rexp -> inferRExp env rexp

    -- RefE _ lexp -> failure x    @TODO: infer for LEXP

    -- RLExp _ lexp -> failure x   @TODO: infer for LEXP
    
    ArrList _ rexps -> case rexps of
        [] -> EM.Bad "Error: empty array initializer."
        _  -> do
            t <- foldl1 supremumM $ map (inferRExp env) rexps
            when (t == TError) (EM.Bad "Error: types in an array initializer are not compatible.")
            return $ TArr (length rexps) t
    
    FCall _ ident rexps -> lookType (idName ident) env

    PredR _ pread -> return $ tctypeOf pread

    Lit _ literal -> return $ tctypeOf literal

    _             -> EM.Bad "INFER OF A NON-COVERED REXP."