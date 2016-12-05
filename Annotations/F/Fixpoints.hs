{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

module Annotations.F.Fixpoints (
    -- * Fixed points of functors
    Fix(..), compos,
    Algebra, cata,
    Coalgebra, ana,
    ErrorAlgebra, cascade,
  ) where

import Annotations.Except

import Data.Monoid
import Data.Traversable
import Control.Monad


-- | Fixpoint of functors.
newtype Fix fT  = In    { out      :: fT (Fix fT)  }

deriving instance Show (f (Fix f)) => Show (Fix f)
-- deriving instance Eq (f (Fix f)) => Eq (Fix f)
--
-- Deriving this instance results in compilation taking forever on GHC 8.0.1
-- and later (see https://ghc.haskell.org/trac/ghc/ticket/12234). This is
-- a workaround until that issue is fixed.
instance Eq (f (Fix f)) => Eq (Fix f) where
  In x == In y = x == y

mapFix :: (f (Fix f) -> g (Fix g)) -> Fix f -> Fix g
mapFix f = In . f . out

-- | Algebras for catamorphisms.
type Algebra fT aT = fT aT -> aT

-- | Reduces a tree to a value according to the algebra.
cata :: Functor fT => Algebra fT aT -> Fix fT -> aT
cata f = f . fmap (cata f) . out

-- | Coalgebras for anamorphisms.
type Coalgebra fT aT = aT -> fT aT

-- | Constructs a tree from a value according to the coalgebra.
ana :: Functor fT => Coalgebra fT aT -> aT -> Fix fT
ana f = In . fmap (ana f) . f

-- | Apply a transformation to a tree's direct children.
compos :: Functor f => (Fix f -> Fix f) -> Fix f -> Fix f
compos = mapFix . fmap

-- | Algebras for error catamorphisms.
type ErrorAlgebra fT eT aT = fT aT -> Either eT aT

-- | Reduces a tree to a value according to the algebra, propagating potential errors.
cascade :: (Traversable fT, Monoid eT) =>
  ErrorAlgebra fT eT aT -> Algebra fT (Except eT aT)
cascade alg expr = case sequenceA expr of
  Failed xs  -> Failed xs
  OK tree'   -> case alg tree' of
    Left xs    -> Failed xs
    Right res  -> OK res
