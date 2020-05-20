module TypeChecker where

import AbsChapel
import TCType
import TCInstances
import Env
import qualified ErrM as EM

import Control.Monad (when)

-- Copied from Skel --------------------------------------
type Result = EM.Err TCType

failure :: Show a => a -> Result
failure x = EM.Bad $ "Undefined case: " ++ show x
----------------------------------------------------------


inferRExp :: Env -> RExp -> EM.Err TCType
inferRExp env rexp = let x = "Dummy" in case rexp of
    Or rexp1 rexp2 -> failure x
    And rexp1 rexp2 -> failure x
    Not rexp -> failure x
    Comp rexp1 compop rexp2 -> failure x
    Arith rexp1 arithop rexp2 -> failure x
    Sign signop rexp -> failure x
    RefE lexp -> failure x
    RLExp lexp -> failure x
    
    ArrList rexps -> case rexps of
        [] -> EM.Bad "Error: empty array initializer."
        _  -> do
            t <- foldl1 supremumM $ map (inferRExp env) rexps
            when (t == TError) (EM.Bad "Error: types in an array initializer are not compatible.")
            return $ TArr (length rexps) t
    
    FCall ident rexps -> failure x

    PredR pread -> return $ tctypeOf pread
    Lit literal -> return $ tctypeOf literal