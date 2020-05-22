module Env where

import qualified Data.Map.Lazy as M
import AbsChapel
import TCType
import qualified ErrM as EM
import Data.Maybe
import Control.Monad (join)

type Id = String
type Env = [Context]
data Context = Context { 
      entryMap  :: M.Map Id Entry
    , returns   :: TCType
    , inWhile   :: Bool
}  

data Entry
    = Var Loc TCType
    | Const Loc TCType Literal
    | Fun Loc TCType


-- Entry have TCType
instance TCTypeable Entry where
    tctypeOf x = case x of
        Var _ t     -> t
        Const _ t _ -> t
        Fun _ t     -> t


-- Take Just the deepest entry mapped from id (if it exists), otherwise Nothing
lookEntry :: Id -> Env -> Maybe Entry
lookEntry id env = join $ head' $ dropWhile isNothing $ map (M.lookup id . entryMap) env

head' :: [a] -> Maybe a
head' []        = Nothing
head' (x:xs)    = Just x

-- Take the Ok TCType from the deepest entry mapped from id (if it exists), otherwise Bad
lookType :: Id -> Env -> EM.Err TCType
lookType id env = case lookEntry id env of
    Nothing -> EM.Bad "Variable not declared."
    Just x  -> EM.Ok $ tctypeOf x


-- Push an empty context on top of the stack
pushContext :: Env -> Env
pushContext (c:cs) = (Context mempty (returns c) (inWhile c)) : (c : cs)

-- Pop context on top of the stack
popContext :: Env -> Env
popContext (c:cs) = cs 