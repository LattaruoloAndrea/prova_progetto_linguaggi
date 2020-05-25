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

type Mutability = Bool
data Entry
    = Var Loc TCType Mutability
    | Const Loc TCType Literal
    | Fun Loc [Param] Intent TCType
    deriving (Show)

isMut :: Entry -> Mutability
isMut (Var _ _ m) = m


data Param = Param Loc Id Intent TCType
    deriving (Show)

-- Convert from an AbsChapel.Form data to Param data
formToParam :: Form -> Param
formToParam (Form it (Ident l n) ty) = Param l n it $ tctypeOf ty

-- Convert a Param to an Entry-Var (immutable when ConstIn or ConstRef modality)
paramToEntry :: Param -> Entry
paramToEntry (Param l id it ty) = Var l ty $ it==ConstIn || it==ConstRef


-- Entry have TCType
instance TCTypeable Entry where
    tctypeOf x = case x of
        Var _ t _   -> t
        Const _ t _ -> t
        Fun _ _ _ t -> t

-- Param have TCType
instance TCTypeable Param where
    tctypeOf (Param _ _ _ t) = t


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
    Just f@(Fun _ _ _ _) -> return f
    _                    -> EM.Bad "Function not declared."

-- Take the Ok (Const ..) from the deepest entry mapped from id (if it exists), otherwise Bad
lookConst :: Id -> Env -> EM.Err Entry
lookConst id env = case lookEntry id env of
    Just c@(Const _ _ _) -> return c
    _                    -> EM.Bad "Constant not declared."


-- Add a new Entry to the deepest context given Id
-- The existence is checked before insertion.
makeEntry :: Env -> (Id, Entry) -> EM.Err Env
makeEntry env@(c:cs) (id, entry) = let cmap = entryMap c in case M.lookup id cmap of
    Nothing -> return $
        let cmap' = M.insert id entry cmap
        in (Context cmap' (returns c) (inWhile c) (inFor c)) : cs
    
    Just x  -> EM.Bad "Error: local name already declared at this scope."

-- Add constant to the deepest context given Ident and Literal value.
-- The existence is checked before insertion.
makeConst :: Env -> Ident -> Literal -> EM.Err Env
makeConst env (Ident l n) lit = makeEntry env (n, Const l (tctypeOf lit) lit)

-- Add variable to the deepest context given Ident, TCType and mutability.
-- The existance is checked before insertion.
makeVar :: Mutability -> Env -> Ident -> TCType -> EM.Err Env
makeVar mut env (Ident l n) t = makeEntry env (n, Var l t mut)

-- Add mutable variable
makeMutable :: Env -> Ident -> TCType -> EM.Err Env
makeMutable = makeVar False

-- Add immutable variable
makeImmutable :: Env -> Ident -> TCType -> EM.Err Env
makeImmutable = makeVar True


-- add a function to the deepest contex given Ident, [Param], return Intent and return TCType
-- The existance is checked before insertion
makeFun :: Env -> Ident -> [Param] -> Intent -> TCType -> EM.Err Env
makeFun env (Ident l n) ps it rt = makeEntry env (n, Fun l ps it rt)


-- Push an empty context on top of the stack
pushContext :: Env -> Env
pushContext env@(c:cs) = (Context mempty (returns c) (inWhile c) (inFor c)) : env

-- Push an empty context on top of the stack for an unbounded iteration (inWhile = True)
pushWhile :: Env -> Env
pushWhile env@(c:cs) = (Context mempty (returns c) True (inFor c)) : env

-- Push an empty context on top of the stack for a bounded iteration (inWhile = False, inFor = True)
pushFor :: Env -> Env
pushFor env@(c:cs) = (Context mempty (returns c) False True) : env

-- Push an empty context on top of the stack for a function declaration (returns from input, inWhile = False, inFor = False)
pushFun :: Env -> TCType -> Env
pushFun env ret = (Context mempty ret False False) : env

-- Pop context on top of the stack
popContext :: Env -> Env
popContext (c:cs) = cs

-- Add a for-loop counter (as immutable integer) to the deepest context given Ident
makeForCounter :: Env -> Ident -> EM.Err Env
makeForCounter env@(c:cs) ident = makeImmutable env ident TInt
