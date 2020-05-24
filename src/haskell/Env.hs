module Env where

import qualified Data.Map.Lazy as M
import AbsChapel
import TCType
import qualified ErrM as EM
import Data.Maybe
import Control.Monad (join)
import TCInstances

type Id = String
type Env = [Context]
data Context = Context { 
      entryMap  :: M.Map Id Entry
    , returns   :: TCType
    , inWhile   :: Bool
    , inFor     :: Bool
} deriving (Show)

data Entry
    = Var Loc TCType
    | Const Loc TCType Literal
    | Fun Loc TCType
    deriving (Show)


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
    Nothing -> EM.Bad "Name not declared."
    Just x  -> return $ tctypeOf x

-- Take the Ok (Fun ..) from the deepest entry mapped from id (if it exists), otherwise Bad
lookFun :: Id -> Env -> EM.Err Entry
lookFun id env = case lookEntry id env of
    Just f@(Fun _ _) -> return f
    _                -> EM.Bad "Function not declared."


-- Add constant to the deepest context given Ident and value.
-- The existance is checked before insertion.
makeConst :: Env -> Ident -> Literal -> EM.Err Env
makeConst env@(c:cs) (Ident l n) lit = let cmap = entryMap c in case M.lookup n cmap of
    Nothing -> return $
        let cmap' = M.insert n (Const (tctypeOf lit) lit) cmap
        in (Context cmap' (returns c) (inWhile c) (inFor c)) : cs
    
    Just x  -> EM.Bad "Error: local name already declared at this scope."

-- Add variable to the deepest context given Ident and TCType.
-- The existance is checked before insertion.
makeVar :: Env -> Ident -> TCType -> EM.Err Env
makeVar env@(c:cs) (Ident l n) t = let cmap = entryMap c in case M.lookup n cmap of
    Nothing -> return $
        let cmap' = M.insert n (Var l t) cmap
        in (Context cmap' (returns c) (inWhile c) (inFor c)) : cs
    
    Just x  -> EM.Bad "Error: local name already declared at this scope."


-- Push an empty context on top of the stack
pushContext :: Env -> Env
pushContext env@(c:cs) = (Context mempty (returns c) (inWhile c) (inFor c)) : env

-- Push an empty context on top of the stack for an unbounded iteration (inWhile = True)
pushWhile :: Env -> Env
pushWhile env@(c:cs) = (Context mempty (returns c) True (inFor c)) : env

-- Push an emmpty context on top of the stack for a bounded iteration (inWhile = False, inFor = True)
pushFor :: Env -> Env
pushFor env@(c:cs) = (Context mempty (returns c) False True) : env

-- Pop context on top of the stack
popContext :: Env -> Env
popContext (c:cs) = cs

-- Add a for-loop counter (as constant with dummy int value) to the deepest context given Ident
makeForCounter :: Env -> Ident -> EM.Err Env
makeForCounter env@(c:cs) ident = makeConst env ident (LInt 0)