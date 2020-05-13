-- BNF Converter: Error Monad
-- Copyright (C) 2004  Author:  Aarne Ranta

-- This file comes with NO WARRANTY and may be used FOR ANY PURPOSE.
module ErrTC where

-- the Error monad: like Maybe type with error msgs

import Control.Monad (MonadPlus(..), liftM)
import Control.Applicative (Applicative(..), Alternative(..))
import Data.DList


-- Ok means checking is without "fatal" errors but there could be warning messages
-- Bad means the type-checking went wrong
-- DList (difference list) are used to achieve constant time appending
data ErrTC a = Ok (DList String) a | Bad (DList String) a
  deriving (Read, Show, Eq, Ord)

instance Monad ErrTC where
  return      = Ok ("" ++)
  Ok a  >>= f = f a
  Bad s >>= _ = Bad s

instance Applicative Err where
  pure = Ok
  (Bad s) <*> _ = Bad s
  (Ok f) <*> o  = liftM f o

instance Functor Err where
  fmap = liftM

instance MonadPlus Err where
  mzero = Bad "Err.mzero"
  mplus (Bad _) y = y
  mplus x       _ = x

instance Alternative Err where
  empty = mzero
  (<|>) = mplus
