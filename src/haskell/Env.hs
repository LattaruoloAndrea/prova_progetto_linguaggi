module Env where

import qualified Data.Map.Lazy as M
import AbsChapel
import TCType

type Id = String
type Env = [Context]
type Context = M.Map Id Entry

data Entry
    = Var Loc TCType
    | Const Loc TCType Literal
    | Fun Loc TCType