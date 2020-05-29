module Env where

import qualified Data.Map.Lazy as M
import AbsChapel
import TCType
import qualified ErrM as EM
import Data.Maybe
import Control.Monad (join)
import TCInstances
import ErrorHandling
import Locatable

type Id = String
type Env = [Context]
data Context = Context { 
      entryMap  :: M.Map Id Entry
    , returns   :: TCType
    , inWhile   :: Bool
    , inFor     :: Bool
    , isRef     :: Bool
} deriving (Show)

type Mutability = Bool
data Entry
    = Var Loc TCType Mutability
    | Const Loc TCType Literal
    | Fun Loc [Param] Intent TCType
    deriving (Show)


data Param = Param Loc Id Intent TCType
    deriving (Show)


paramsOf :: Entry -> [Param]
paramsOf (Fun _ ps _ _) = ps

isMut :: Entry -> Mutability
isMut (Var _ _ m)   = m
isMut (Const _ _ _) = False
isMut (Fun _ _ _ _) = True  -- The check for mutability of functions does not have much sense
                            -- it is assumed mutable for a practical reason on simplifying error
                            -- check on assignments


isFun :: Entry -> Bool
isFun (Fun _ _ _ _) = True
isFun _             = False


isLExp :: RExp -> Bool
isLExp (RLExp _ _)  = True
isLExp _            = False


-- Returns the mutability of an LExp
-- If the entry does not exist, it is assumed to be mutable
-- (although it would be better to check the existence first)
isMutable :: Env -> LExp -> Mutability
isMutable env l = case l of
    Deref l     -> isMutable env l

    Access l _  -> isMutable env l

    Name ident  -> case lookEntry ident env of
        Just entry  -> isMut entry
        _           -> False


-- Returns True if a name is a function in the current environment
-- If the name is a Const or Var, False
-- Otherwise, if the name is not defined it is assumed True (for ease of error-checking)
isFunction :: Env -> LExp -> Bool
isFunction env l = case l of
    Name ident -> case lookEntry ident env of
        Just entry  -> isFun entry
        Nothing     -> True
    
    _               -> False





instance Locatable Entry where
    locOf x = case x of
        Var l _ _   -> l
        Const l _ _ -> l
        Fun l _ _ _ -> l

instance Locatable Param where
    locOf (Param l _ _ _) = l




-- Convert from an AbsChapel.Form data to Param data
formToParam :: Form -> Param
formToParam (Form it (Ident l n) ty) = Param l n it $ tctypeOf ty

-- Convert a Param to an Entry-Var (immutable when ConstIn or ConstRef modality)
paramToEntry :: Param -> Entry
paramToEntry (Param l id it ty) = Var l ty $ not $ it==ConstIn || it==ConstRef


-- Get the Ident from Param
identFromParam :: Param -> Ident
identFromParam (Param l id _ _) = Ident l id


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
lookEntry :: Ident -> Env -> Maybe Entry
lookEntry (Ident _ id) env = join $ head' $ dropWhile isNothing $ map (M.lookup id . entryMap) env

head' :: [a] -> Maybe a
head' []        = Nothing
head' (x:xs)    = Just x

-- Take the Ok TCType from the deepest entry mapped from id (if it exists), otherwise Bad
lookType :: Ident -> Env -> EM.Err TCType
lookType id env = case lookEntry id env of
    Nothing -> errorNameDoesNotExist id
    Just x  -> return $ tctypeOf x

-- Take the Ok (Fun ..) from the deepest entry mapped from id (if it exists), otherwise Bad
lookFun :: Ident -> Env -> EM.Err Entry
lookFun id env = case lookEntry id env of
    Just f@(Fun _ _ _ _) -> return f
    _                    -> errorFunDoesNotExist id

-- Take the Ok (Const ..) from the deepest entry mapped from id (if it exists), otherwise Bad
lookConst :: Ident -> Env -> EM.Err Entry
lookConst id env = case lookEntry id env of
    Just c@(Const _ _ _) -> return c
    _                    -> errorConstDoesNotExist id


-- Add a new Entry to the deepest context given Id
-- The existence is checked before insertion.
makeEntry :: Env -> (Ident, Entry) -> EM.Err Env
makeEntry env@(c:cs) (ident@(Ident _ id), entry) = let cmap = entryMap c in case M.lookup id cmap of
    Nothing -> return $
        let cmap' = M.insert id entry cmap
        in (Context cmap' (returns c) (inWhile c) (inFor c) (isRef c)) : cs
    
    Just x  -> errorNameAlreadyDeclared ident $ locOf x

-- Add constant to the deepest context given Ident and Literal value.
-- The existence is checked before insertion.
makeConst :: Env -> Ident -> Literal -> EM.Err Env
makeConst env id@(Ident l n) lit = makeEntry env (id, Const l (tctypeOf lit) lit)

-- Add variable to the deepest context given Ident, TCType and mutability.
-- The existance is checked before insertion.
makeVar :: Mutability -> Env -> Ident -> TCType -> EM.Err Env
makeVar mut env id@(Ident l n) t = if t == TVoid
    then badLoc l "Variable cannot be of type void"
    else makeEntry env (id, Var l t mut)

-- Add mutable variable
makeMutable :: Env -> Ident -> TCType -> EM.Err Env
makeMutable = makeVar True

-- Add immutable variable
makeImmutable :: Env -> Ident -> TCType -> EM.Err Env
makeImmutable = makeVar False


-- add a function to the deepest contex given Ident, [Param], return Intent and return TCType
-- The existance is checked before insertion
makeFun :: Env -> Ident -> [Param] -> Intent -> TCType -> EM.Err Env
makeFun env id@(Ident l n) ps it rt = makeEntry env (id, Fun l ps it rt)


-- Push an empty context on top of the stack
pushContext :: Env -> Env
pushContext env@(c:cs) = (Context mempty (returns c) (inWhile c) (inFor c) (isRef c)) : env

-- Push an empty context on top of the stack for an unbounded iteration (inWhile = True)
pushWhile :: Env -> Env
pushWhile env@(c:cs) = (Context mempty (returns c) True (inFor c) (isRef c)) : env

-- Push an empty context on top of the stack for a bounded iteration (inWhile = False, inFor = True)
pushFor :: Env -> Env
pushFor env@(c:cs) = (Context mempty (returns c) False True (isRef c)) : env

-- Push an empty context on top of the stack for a function declaration
-- (returns from input, inWhile = False, inFor = False, isRef from Intent)
pushFun :: Env -> TCType -> Intent -> Env
pushFun env ret it = (Context mempty ret False False (it == Ref)) : env

-- Pop context on top of the stack
popContext :: Env -> Env
popContext (c:cs) = cs

-- Add a for-loop counter (as immutable integer) to the deepest context given Ident
makeForCounter :: Env -> Ident -> EM.Err Env
makeForCounter env@(c:cs) ident = makeImmutable env ident TInt
