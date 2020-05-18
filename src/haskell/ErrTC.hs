-- BNF Converter: Error Monad
-- Copyright (C) 2004  Author:  Aarne Ranta

-- This file comes with NO WARRANTY and may be used FOR ANY PURPOSE.
module ErrTC where

-- the Error monad: like Maybe type with error msgs

-- MODIFIED VERSION FOR APPENDING WARNING MESSAGES THAT DO NOT STOP COMPUTATION

import Control.Monad (MonadPlus(..), liftM, ap)
import Control.Applicative (Applicative(..), Alternative(..))
import qualified Data.DList as DL -- Difference list for O(1) appending

type Log = DL.DList String

data Err a = Ok {msg :: Log, ans :: a} | Bad {msg :: Log, ans :: a}
  deriving (Read, Show, Eq, Ord)

instance Monad Err where
  return      = Ok (DL.fromList [])
  Ok s a  >>= f = case f a of
    Ok s' a' -> Ok (s `mappend` s') a'
    Bad s' a' -> Bad (s `mappend` s') a'
  Bad s a >>= f = case f a of
    Ok s' a' -> Bad (s `mappend` s') a'
    Bad s' a' -> Bad (s `mappend` s') a'

instance Applicative Err where
  pure = return
  (<*>) = ap

instance Functor Err where
  fmap = liftM


bad :: String -> a -> Err a
bad s a = Bad (DL.fromList [s]) a


-- Cannot define suitable mzero/empty
-- to work with guard

-- instance MonadPlus Err where
--   mzero = Bad "Err.mzero"
--   mplus (Bad _) y = y
--   mplus x       _ = x

-- instance Alternative Err where
--   empty = mzero
--   (<|>) = mplus
