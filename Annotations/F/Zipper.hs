{-# LANGUAGE RankNTypes #-}

module Annotations.F.Zipper
  ( Zipper(..), enter, leave, child, allFoci, Nav(..),
  ) where

import Annotations.F.Fixpoints
import Data.Foldable
import Data.Maybe
import Control.Monad
import Data.Monoid

-- | A quasi-zipper, meant for O(1), fixed-memory stepping through a tree structure, but not meant for updates.
data Zipper a = Zipper
  { zFocus :: a                 -- ^ The current focus of this zipper.
  , zUp    :: Maybe (Zipper a)  -- ^ Move up to the parent.
  , zLeft  :: Maybe (Zipper a)  -- ^ Move to the left sibling.
  , zRight :: Maybe (Zipper a)  -- ^ Move to the right sibling.
  , zDown  :: Maybe (Zipper a)  -- ^ Move down into the leftmost child.
  }

-- | Captures navigation steps in a 'Zipper'. Its 'Monoid' instance specifies the identity step ('mempty') and step composition ('mappend').
newtype Nav = Nav { nav :: forall a. Zipper a -> Maybe (Zipper a) }

instance Monoid Nav where
  mempty = Nav return
  mappend (Nav n1) (Nav n2) = Nav (n1 >=> n2)

-- | Move into the root of the fixed point. The returned zipper builds a data structure with optimal sharing and fixed memory usage. For example, @zLeft >=> zRight@ (if successful) returns to the same node in memory.
enter :: Foldable f => Fix f -> Zipper (Fix f)
enter f = fromJust (enter' Nothing Nothing [f])

enter' :: Foldable f =>
  Maybe (Zipper (Fix f)) ->
  Maybe (Zipper (Fix f)) ->
  [Fix f] ->
  Maybe (Zipper (Fix f))
enter' _  _    [] = Nothing
enter' up left (focus@(In f) : fs) = here
  where
    here   = Just (Zipper focus up left right down)
    right  = enter' up here fs
    down   = enter' here Nothing (toList f)

-- | Walk back up to the root of the fixed point and leave the zipper structure.
leave :: Zipper a -> a
leave z = maybe (zFocus z) leave (zUp z)

-- | Move down into a specific child.
child :: Int -> Zipper a -> Maybe (Zipper a)
child 0 = zDown
child n = child (n - 1) >=> zRight

-- | Traverses the tree in preorder, yielding all possible tree selections.
allFoci :: Foldable f => Fix f -> [Zipper (Fix f)]
allFoci = allFoci' . enter

allFoci' :: Foldable f => Zipper (Fix f) -> [Zipper (Fix f)]
allFoci' z = z : [ z'' | Just z' <- [zDown z, zRight z], z'' <- allFoci' z' ]
