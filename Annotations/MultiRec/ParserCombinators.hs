{-# LANGUAGE FlexibleContexts #-}

module Annotations.MultiRec.ParserCombinators (YP, mkBounded, unit, chainr, chainl) where

import Annotations.Bounds
import Annotations.BoundsParser
import Annotations.MultiRec.Yield
import Control.Monad.Trans

import qualified Text.Parsec as P
import Generics.MultiRec hiding (show)

import Data.Function

-- | A parser that yields its components, annotated with 'Bounds'.
type YP s fam m = P s (YieldT Bounds fam m)

-- | Wrap an unnotated tree with position information from the parse state.
unit :: (Fam fam, EqS fam, HFunctor fam (PF fam), Monad m) =>
  fam a ->
  YP s fam m a ->
  YP s fam m a
unit w p = do
  left <- getPos
  x <- p
  mkBounded w left x

-- | Given the left margin of a structure, asks the parser for the right
--   margin and wraps the position information around the root of the tree.
mkBounded :: (Fam fam, EqS fam, HFunctor fam (PF fam), Monad m) => fam a -> Range -> a -> YP s fam m a
mkBounded w left x = do
  right <- getPos
  lift $ yield w (Bounds left right) x

-- | Parse right-recursive structures.
chainr :: (Fam fam, EqS fam, HFunctor fam (PF fam), Monad m, Show a) =>
  fam a -> YP s fam m a -> YP s fam m (a -> a -> a) -> YP s fam m a
chainr w px pf = fix $ \loop -> do
  left <- getPos
  x <- px
  P.option x $ do
    f <- pf
    y <- loop
    mkBounded w left (f x y)

-- | Parse left-recursive structures.
chainl :: (Fam fam, EqS fam, HFunctor fam (PF fam), Monad m, Show a) =>
  fam a -> YP s fam m a -> YP s fam m (a -> a -> a) -> YP s fam m a
chainl w px pf = do
    left <- getPos
    px >>= rest left
  where
    rest left = fix $ \loop x -> P.option x $ do
      f <- pf
      y <- px
      mkBounded w left (f x y) >>= loop
