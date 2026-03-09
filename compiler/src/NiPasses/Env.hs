{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}
module NiPasses.Env
  ( Env(..)
  , makeEnv
  , lookupEnv
  , extendEnv
  , fromList
  ) where

import qualified Data.Map as M
import qualified GHC.Exts as Exts

-- =====================================================
-- ENVIRONMENT (SYMBOL TABLE)
-- =====================================================
-- This module is used by COMPILER PASSES.
-- It maps names to metadata (usually other names).
-- It NEVER stores runtime values.
-- =====================================================

newtype Env a = Env (M.Map String a)
  deriving (Eq, Show)

instance Exts.IsList (Env a) where
  type Item (Env a) = (String, a)
  fromList pairs = Env (M.fromList pairs)
  toList (Env m)  = M.toList m

makeEnv :: Env a
makeEnv = Env M.empty

lookupEnv :: String -> Env a -> Maybe a
lookupEnv x (Env m) = M.lookup x m

extendEnv :: String -> a -> Env a -> Env a
extendEnv x v (Env m) = Env (M.insert x v m)

fromList :: [(String, a)] -> Env a
fromList pairs = Env (M.fromList pairs)