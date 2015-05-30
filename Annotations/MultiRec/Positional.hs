{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes            #-}

module Annotations.MultiRec.Positional where

import Annotations.Bounds
import Annotations.ExploreHints
import Annotations.MultiRec.Annotated
import Annotations.MultiRec.Zipper
import Annotations.MultiRec.ZipperFix
import Generics.MultiRec.Base
import Generics.MultiRec.HFunctor

import Data.Maybe
import Data.List (sortBy)
import Data.Ord
import Control.Applicative


-- | Find the deepest node whose bounds match the given range. See 'rangeInBounds'.
selectByRange :: Zipper phi (PF phi) =>
   phi ix -> Range -> AnnFix Bounds phi ix ->
   Maybe (AnnZipper phi Bounds ix)
selectByRange p range@(left, _) = listToMaybe . reverse . explore p hints where
  hints bounds@(Bounds _ (ir, _)) = 
      ExploreHints
        {  matchHere     = range `rangeInBounds` bounds
        ,  exploreDown   = range `rangeInRange` outerRange bounds
        ,  exploreRight  = left >= ir
        }

selectByPos :: (Zipper phi (PF phi)) =>
  phi ix -> Int -> AnnFix Bounds phi ix ->
  Maybe (AnnZipper phi Bounds ix)
selectByPos p pos = findLeftmostDeepest p (posInRange pos . innerRange)

repairBy :: (Ord dist, HFunctor phi (PF phi)) =>
  phi ix -> (Range -> Range -> dist) -> AnnFix Bounds phi ix -> Range -> Bounds
repairBy p cost tree range =
  head (sortOn (cost range . innerRange) (allAnnotations p tree))

-- | Defined as @'repairBy' 'distRange'@.
repair :: HFunctor phi (PF phi) =>
  phi ix -> AnnFix Bounds phi ix -> Range -> Bounds
repair p = repairBy p distRange

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn = sortBy . comparing

-- | Move around in a tree according to the 'Nav', expressed in tree selections. Although a 'Range' is required as input, a 'Bounds' is returned, providing information about all the valid text selections that would select the particular tree node.
moveSelection :: Zipper phi (PF phi) => phi ix -> AnnFix Bounds phi ix -> Nav -> Range -> Maybe Bounds
moveSelection p tree nav range = focusAnn <$> (selectByRange p range tree >>= nav)
