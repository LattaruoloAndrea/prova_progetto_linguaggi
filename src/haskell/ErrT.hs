-- BNF Converter: Error Monad
-- Copyright (C) 2004  Author:  Aarne Ranta

-- This file comes with NO WARRANTY and may be used FOR ANY PURPOSE.
module ErrT where

-- the Error monad: like Maybe type with error msgs

import Control.Monad (MonadPlus(..), liftM, ap)
import Control.Applicative (Applicative(..), Alternative(..))
import qualified Data.DList as DL -- Difference list for O(1) appending
import ErrM

type Log = DL.DList String

data ErrT a = OkT a | BadT a Log
  deriving (Read, Show, Eq, Ord)


-- Transform an ErrT to a BNFC-Err
fromErrT :: ErrT a -> Err a
fromErrT (OkT a) = Ok a
fromErrT (BadT _ s) = Bad . unlines $ DL.toList s

-- Transform a BNFC-Err in an ErrT
-- a 'basic' value is provided for BadT
toErrT :: a -> Err a -> ErrT a
toErrT _ (Ok a)  = OkT a
toErrT a (Bad s) = BadT a $ DL.fromList [s]

-- Transform a BNFC-Err in an ErrT
-- a Err value is provided for BadT
-- also, a 'basic' value is provided in case the first one is Bad
toErrTM :: a -> Err a -> Err a -> ErrT a
toErrTM _ _ (Ok a)    = OkT a
toErrTM _ (Ok a') bad = toErrT a' bad
toErrTM a bad1 bad2   = toErrT a bad2

instance Monad ErrT where
  return         = OkT
  OkT a    >>= f = f a
  BadT a s >>= f = case f a of
      OkT a'     -> BadT a' s
      BadT a' s' -> BadT a' $ s `mappend` s'

instance Applicative ErrT where
  pure = OkT
  (<*>) = ap

instance Functor ErrT where
  fmap = liftM

-- instance MonadPlus Err where
--   mzero = Bad "Err.mzero"
--   mplus (Bad _) y = y
--   mplus x       _ = x

-- instance Alternative Err where
--   empty = mzero
--   (<|>) = mplus
