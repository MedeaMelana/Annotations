module Annotations.F.Annotated (
    -- * Annotations
    Ann(..), AnnFix, rootAnn, AnnFix1, mkAnnFix, unannotate, errorCata,
    -- * Exploring annotated trees using zippers
    explore, findLeftmostDeepest,
  ) where


import Annotations.Except
import Annotations.ExploreHints
import Annotations.F.Fixpoints
import Annotations.F.Zipper

import Data.Foldable (Foldable, foldMap)
import Data.Traversable
import Data.Maybe
import Control.Applicative
import Control.Monad


-- | Lifted annotation of functors.
data Ann x f a = Ann x (f a)
  deriving (Eq, Show)

instance Functor f => Functor (Ann x f) where
  fmap f (Ann x t) = Ann x (fmap f t)

instance Foldable f => Foldable (Ann x f) where
  foldMap f (Ann _ t) = foldMap f t

instance Traversable f => Traversable (Ann x f) where
  traverse f (Ann x t) = Ann x <$> traverse f t

-- | A fully annotated tree.
type AnnFix   xT fT  = Fix (Ann xT fT)

-- | A functor with fully annotated children.
type AnnFix1  xT fT  = fT (AnnFix xT fT)

-- | Supply a tree with an annotation at the top level.
mkAnnFix :: x -> AnnFix1 x f -> AnnFix x f
mkAnnFix x = In . Ann x

-- | Yields the annotation at the root of the tree.
rootAnn :: AnnFix x f -> x
rootAnn (In (Ann x _)) = x

-- | Recursively discard annotations.
unannotate :: Functor f => AnnFix x f -> Fix f
unannotate (In (Ann _ tree)) = In (fmap unannotate tree)

-- | Reduces a tree to a value according to the algebra, collecting potential
--   errors. The errors are combined with the annotations in the tree at the
--   positions at which the errors occurred.
errorCata :: Traversable fT => ErrorAlgebra fT eT aT -> AnnFix xT fT -> Except [(eT, xT)] aT
errorCata alg (In (Ann x expr)) =
  case traverse (errorCata alg) expr of
    Failed xs  -> Failed xs
    OK expr'   -> case alg expr' of
      Left x'   -> Failed [(x', x)]
      Right v   -> OK v

-- | Explore an annotated tree. Starting with the root of the tree, at each 
-- position the annotation at that position is matched against the 
-- 'ExploreHints' predicates and all the selections where 'matchHere' was 
-- positive are collected. The 'exploreRight' and 'exploreDown' allow pruning 
-- of the tree, preventing entire parts from being visited.
explore :: Foldable f => (x -> ExploreHints) -> AnnFix x f -> [Zipper (AnnFix x f)]
explore hints = explore' hints . enter

explore' :: Foldable f => (x -> ExploreHints) -> Zipper (AnnFix x f) -> [Zipper (AnnFix x f)]
explore' hints root = [ z | (dirOk, zs) <- dirs, dirOk (hints x), z <- zs ]
  where
    In (Ann x _) = zFocus root
    dirs =
      [  (matchHere,     [root])
      ,  (exploreDown,   exploreMore (zDown   root))
      ,  (exploreRight,  exploreMore (zRight  root))
      ]
    exploreMore = maybe [] (explore' hints)

-- | Find the deepest node in an annotated tree that matches the predicate.
-- Starting with the root, the predicate tells whether a node's annotation
-- matches. If so, the search continues at the node's children and the node's
-- siblings to the right are excluded from further exploration. If no child
-- matches, the node itself is returned.
findLeftmostDeepest :: Foldable f =>
  (x -> Bool) -> AnnFix x f -> Maybe (Zipper (AnnFix x f))
findLeftmostDeepest down = listToMaybe . reverse . explore hints
  where
    hints x
      | down x     = ExploreHints  True   True   False
      | otherwise  = ExploreHints  False  False  True
