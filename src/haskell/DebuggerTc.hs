module DebuggerTc where

import AbsChapel
import TypeChecker
import TCType
import LexChapel
import ParChapel
import AbsChapel
import TCType
import TCInstances
import Locatable
import Env
import Control.Monad (unless, when)
import qualified ErrM as EM
import qualified ErrT as ET

-- take a rexp and transform it in abstact syntax

makeToken :: String -> [Token]
makeToken gr = tokens gr

fullRexp :: String -> (EM.Err Program)
fullRexp gr = let tok = tokens gr in  --case tokens of
          pProgram tok

createInit :: String -> String -> String
createInit typee gr = "var x:"++ typee ++"="++gr++";"

easyRexp :: String-> String -> (EM.Err Program)
easyRexp typee gr = let tok = tokens (createInit typee gr) in  --case tokens of
          pProgram tok

takeRexp :: (EM.Err Program) -> RExp
takeRexp p = case p of
    EM.Ok g -> takeRexp1 g

takeRexp1 :: Program -> RExp
takeRexp1 p = case p of
    Prog (x:xs) -> takeRexp2 x

takeRexp2 :: Decl -> RExp
takeRexp2 p = case p of
    VList (x:xs) -> takeRexp3 x

takeRexp3 :: VDecl -> RExp
takeRexp3 p = case p of
    Init a b c -> c

-- takeRexp (easyRexp "int" "5+6")

profRExp :: String -> EM.Err TCType
profRExp expr = inferRExp [] (takeRexp (easyRexp "int" expr))
--profRExp "5+6"