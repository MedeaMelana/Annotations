-- | Queries on trees annotated with position information.
module Annotations.F.Positional
  ( selectByRange, selectByPos, validBounds, repairBy, repair, moveSelection
  ) where

import Annotations.Bounds
import Annotations.ExploreHints
import Annotations.F.Fixpoints
import Annotations.F.Zipper
import Annotations.F.Annotated

import Data.Foldable (Foldable, toList)
import Data.Ord
import Data.List (sortBy)
import Data.Maybe
import Control.Applicative

-- | Find the deepest node whose bounds match the given range. See 'rangeInBounds'.
selectByRange :: Foldable f => Range -> AnnFix Bounds f -> Maybe (Zipper (AnnFix Bounds f))
selectByRange range@(left, _) = listToMaybe . reverse . explore hints where
  hints bounds@(Bounds _ (ir, _)) = 
      ExploreHints
        {  matchHere     = range `rangeInBounds` bounds
        ,  exploreDown   = range `rangeInRange` outerRange bounds
        ,  exploreRight  = left >= ir
        }

-- | Find the deepest node whose bounds contain the given position.
selectByPos :: Foldable f => Int -> AnnFix Bounds f -> Maybe (Zipper (AnnFix Bounds f))
selectByPos pos = findLeftmostDeepest (posInRange pos . innerRange)

-- | Find all selections in the tree and return their bounds. The tree is traversed in preorder. Consequently, the bounds are returned in lexicographical order.
validBounds :: Foldable f => AnnFix Bounds f -> [Bounds]
validBounds (In (Ann b f)) = b : concatMap validBounds (toList f)

-- | @repairBy cost tree range@ finds the the closest valid text selection to @range@, where ''closest'' is determined by the specified cost function.
repairBy :: (Foldable f, Ord dist) =>
  (Range -> Range -> dist) -> AnnFix Bounds f -> Range -> Bounds
repairBy cost tree range =
  head (sortOn (cost range . innerRange) (validBounds tree))

-- | Defined as @'repairBy' 'distRange'@.
repair :: Foldable f => AnnFix Bounds f -> Range -> Bounds
repair = repairBy distRange

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn = sortBy . comparing

-- | Move around in a tree according to the 'Nav', expressed in tree selections. Although a 'Range' is required as input, a 'Bounds' is returned, providing information about all the valid text selections that would select the particular tree node.
moveSelection :: Foldable f => AnnFix Bounds f -> Nav -> Range -> Maybe Bounds
moveSelection tree (Nav nav) range = (rootAnn . zFocus) <$> (selectByRange range tree >>= nav)
