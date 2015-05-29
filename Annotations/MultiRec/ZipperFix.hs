{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

-- | Zipper functions specialised to work on fixpoints of pattern functors.
module Annotations.MultiRec.ZipperFix (
    Nav, FixZipper,
    down, down', up, right, left,
    dfnext, dfprev,
    leave, 
  ) where

import Annotations.MultiRec.Zipper
import Generics.MultiRec.HFix

import Prelude hiding (last)
import Data.Maybe

-- | A location within a fixpoint.
type FixZipper phi f = Loc phi f (HFix f)

-- | A navigation step in a fixpoint.
type Nav = forall phi f ix. Zipper phi f =>
  FixZipper phi f ix -> Maybe (FixZipper phi f ix)

-- | Move down to the leftmost child. Returns 'Nothing' if the
-- current focus is a leaf.
down            :: Nav

-- | Move down to the rightmost child. Returns 'Nothing' if the
-- current focus is a leaf.
down'           :: Nav

-- | Move up to the parent. Returns 'Nothing' if the current
-- focus is the root.
up              :: Nav

-- | Move to the right sibling. Returns 'Nothing' if the current
-- focus is the rightmost sibling.
right           :: Nav

-- | Move to the left sibling. Returns 'Nothing' if the current
-- focus is the leftmost sibling.
left            :: Nav

down  (Loc p (HIn x) s      ) = first (\p' z c  -> Loc p' z (Push p c  s)) x
down' (Loc p (HIn x) s      ) = last  (\p' z c  -> Loc p' z (Push p c  s)) x
up    (Loc p x Empty        ) = Nothing
up    (Loc p x (Push p' c s)) = return (Loc p' (HIn $ fill p c x) s)
right (Loc p x Empty        ) = Nothing
right (Loc p x (Push p' c s)) = next (\p z c' -> Loc p z (Push p' c' s)) p c x
left  (Loc p x Empty        ) = Nothing
left  (Loc p x (Push p' c s)) = prev (\p z c' -> Loc p z (Push p' c' s)) p c x

-- ** Derived navigation.

df :: (a -> Maybe a) -> (a -> Maybe a) -> (a -> Maybe a) -> a -> Maybe a
df d u lr l =
    case d l of
      Nothing -> df' l
      r       -> r
  where
    df' l =
      case lr l of
        Nothing ->
          case u l of
            Nothing -> Nothing
            Just l' -> df' l'
        r       -> r

-- | Move through all positions in depth-first left-to-right order.
dfnext :: Nav
dfnext = df down up right

-- | Move through all positions in depth-first right-to-left order.
dfprev :: Nav
dfprev = df down' up left

-- | Return the entire value, independent of the current focus.
leave :: Zipper phi f => Loc phi f (HFix f) ix -> HFix f ix
leave (Loc p x Empty) = x
leave loc             = leave (fromJust (up loc))
