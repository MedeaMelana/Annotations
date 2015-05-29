{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Annotations.MultiRec.Annotated (
    AnnFix, AnnFix1, mkAnnFix, AnyAnnFix,
    unannotate, children, flatten, filterAnnFix, debugFlatten, allAnnotations, 
    AnnZipper, focusAnn,
    explore, findLeftmostDeepest
  ) where

import Annotations.ExploreHints
import Annotations.MultiRec.ShowFam
import Annotations.MultiRec.Any

import Control.Monad.Writer (Writer, execWriter, tell)

import Generics.MultiRec hiding (show)
import Generics.MultiRec.HFix
import Annotations.MultiRec.Zipper
import Annotations.MultiRec.ZipperFix

import Data.Maybe


-- | A fully annotated tree.
type AnnFix x s = HFix (K x :*: PF s)

-- | A functor with fully annotated children.
type AnnFix1 x s = (PF s) (AnnFix x s)

-- | Supply a tree with an annotation at the top level.
mkAnnFix :: x -> AnnFix1 x s ix -> AnnFix x s ix
mkAnnFix x = HIn . (K x :*:)

-- | A fixpoint of a data family @s@ annotated with an @x@ at every recursive position, with existentially quantified top-level index.
type AnyAnnFix x s = AnyF s (AnnFix x s)

-- | Removes all annotations from a recursively annotated fixpoint.
unannotate :: HFunctor s (PF s) => s ix -> AnnFix x s ix -> HFix (PF s) ix
unannotate p = HIn . hmap unannotate p . snd' . hout

-- fst' :: (f :*: g) r ix -> f r ix
-- fst' (x :*: _) = x

snd' :: (f :*: g) r ix -> g r ix
snd' (_ :*: y) = y

-- | Collects the direct children of a functor in a list.
children :: HFunctor s f => s ix -> f r ix -> [AnyF s r]
children p x = execWriter (hmapM collect p x) where
  collect :: s ix -> r ix -> Writer [AnyF s r] (r ix)
  collect w x = tell [AnyF w x] >> return x

-- | Flatten an annotated tree to a list of subtrees coupled with their annotations.
flatten :: forall s x ix. (HFunctor s (PF s), Fam s) => s ix -> AnnFix x s ix -> [(x, Any s)]
flatten p tree@(HIn (K x :*: y)) = (x, Any p (hto p (unannotate p tree :: HFix (PF s) ix))) :
    concatMap (flatten $?) (children p y)

-- | Yield all subtrees whose annotation matches the predicate.
filterAnnFix :: (Fam s, HFunctor s (PF s)) => s ix -> (x -> Bool) -> AnnFix x s ix -> [(x, Any s)]
filterAnnFix p f = filter (f . fst) . flatten p

-- | Flatten an annotated tree and print all subtrees to stdout.
debugFlatten :: (HFunctor s (PF s), ShowFam s, Show x, Fam s) => s ix -> AnnFix x s ix -> IO ()
debugFlatten p = putStr . unlines . map show . flatten p

-- | Recursively yield all annotations in the tree in preorder.
allAnnotations :: HFunctor phi (PF phi) => phi ix -> AnnFix x phi ix -> [x]
allAnnotations p y = f (AnyF p y)
  where
    f (AnyF p' (HIn (K x :*: y))) = x : concatMap f (children p' y)


-- | Extract the annotation of the current focus.
focusAnn :: Loc phi f (HFix (K x :*: g)) ix -> x
focusAnn = on (\_ (HIn (K x :*: _)) -> x)

type AnnZipper phi x = FixZipper phi (K x :*: PF phi)

-- | Explore an annotated tree. Starting with the root of the tree, at each 
-- position the annotation at that position is matched against the 
-- 'ExploreHints' predicates and all the selections where 'matchHere' was 
-- positive are collected. The 'exploreRight' and 'exploreDown' allow pruning 
-- of the tree, preventing entire parts from being visited.
explore :: Zipper phi (PF phi) =>
  phi ix -> (x -> ExploreHints) -> (AnnFix x phi) ix -> [AnnZipper phi x ix]
explore p hints = explore' hints . enter p

explore' :: Zipper phi (PF phi) =>
  (x -> ExploreHints) -> AnnZipper phi x ix -> [AnnZipper phi x ix]
explore' hints root = [ z | (dirOk, zs) <- dirs, dirOk (hints x), z <- zs ]
  where
    x = focusAnn root
    dirs =
      [  (matchHere,     [root])
      ,  (exploreDown,   exploreMore (down   root))
      ,  (exploreRight,  exploreMore (right  root))
      ]
    exploreMore = maybe [] (explore' hints)

-- | Find the deepest node in an annotated tree that matches the predicate.
-- Starting with the root, the predicate tells whether a node's annotation
-- matches. If so, the search continues at the node's children and the node's
-- siblings to the right are excluded from further exploration. If no child
-- matches, the node itself is returned.
findLeftmostDeepest
  :: (Zipper phi (PF phi))
  => phi ix
  -> (x -> Bool)
  -> AnnFix x phi ix
  -> Maybe (AnnZipper phi x ix)
findLeftmostDeepest p down = listToMaybe . reverse . explore p hints
  where
    hints x
      | down x     = ExploreHints  True   True   False
      | otherwise  = ExploreHints  False  False  True
