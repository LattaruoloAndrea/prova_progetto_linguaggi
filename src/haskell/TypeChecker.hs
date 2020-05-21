module TypeChecker where

import AbsChapel
import TCType
import TCInstances
import Locatable
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
    Or _ rexp1 rexp2 -> failure x
    And _ rexp1 rexp2 -> failure x
    Not _ rexp -> failure x
    Comp _ rexp1 compop rexp2 -> failure x
    Arith _ rexp1 arithop rexp2 -> failure x
    Sign _ signop rexp -> failure x
    RefE _ lexp -> failure x
    RLExp _ lexp -> failure x
    
    ArrList _ rexps -> case rexps of
        [] -> EM.Bad "Error: empty array initializer."
        _  -> do
            t <- foldl1 supremumM $ map (inferRExp env) rexps
            when (t == TError) (EM.Bad "Error: types in an array initializer are not compatible.")
            return $ TArr (length rexps) t
    
    FCall _ ident rexps -> failure x

    PredR _ pread -> return $ tctypeOf pread
    Lit _ literal -> return $ tctypeOf literal