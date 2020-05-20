module Env where

import qualified Data.Map.Lazy as M
import AbsChapel

type Id = String
type Env = [Context]

data Context = M.Map Id Entry
data Entry
    = Var Loc TCType
    | Const Loc TCType Literal
    | Fun Loc TCType