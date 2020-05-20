module TypeChecker where

import AbsChapel
import TCType
import TCInstances
import Env
import qualified ErrM as EM



-- inferRExp :: RExp -> Env -> EM.Err TCType
-- inferRExp rexp env = case rexp of
--     Or rexp1 rexp2 -> if (inferRExp rexp1 env) `compare'` (inferRExp rexp2 env) == TNotComparable
--         then TError         -- Add error msg
--         else TError
         
--     And rexp1 rexp2 -> inferLogical rexp1 rexp2 env

--     Not rexp -> if inferRExp
--     Comp rexp1 compop rexp2 -> failure x
--     Arith rexp1 arithop rexp2 -> failure x
--     Sign signop rexp -> failure x
--     RefE lexp -> failure x
--     RLExp lexp -> failure x
--     ArrList rexps -> failure x
--     FCall ident rexps -> failure x
--     PredR pread -> return $ tctypeOf literal
--     Lit literal -> return $ tctypeOf literal