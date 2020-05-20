module TypeChecker where

import AbsChapel
import TCType
import TCInstances
import Env



inferRExp :: RExp -> Env -> TCType
inferRExp rexp env = case rexp of
    Or rexp1 rexp2 -> failure x
    And rexp1 rexp2 -> failure x
    Not rexp -> failure x
    Comp rexp1 compop rexp2 -> failure x
    Arith rexp1 arithop rexp2 -> failure x
    Sign signop rexp -> failure x
    RefE lexp -> failure x
    RLExp lexp -> failure x
    ArrList rexps -> failure x
    FCall ident rexps -> failure x
    PredR pread -> failure x
    Lit literal -> failure x