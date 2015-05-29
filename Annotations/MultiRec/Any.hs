{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Pattern functors existentially quantified in their top-level index type @ix@.
module Annotations.MultiRec.Any
  ( Any(..), mkAny, matchAny
  , AnyF(..), mkAnyF, matchAnyF, unwrapAnyF, ($?)
  ) where

import Annotations.MultiRec.ShowFam
import Generics.MultiRec

-- | A value of some type in data family @s@, together with its witness.
data Any s where
  Any :: s ix -> ix -> Any s

instance ShowFam s => Show (Any s) where
  show = showAny

showAny :: ShowFam s => Any s -> String
showAny (Any w x) = showFam w x

-- | Helper constructor.
mkAny :: El s ix => ix -> Any s
mkAny = Any proof

-- | Unify an 'Any' with an @a@.
matchAny :: forall s a. EqS s => s a -> Any s -> Maybe a
matchAny p (Any w x) = match' w x p where
  match' :: forall b. s b -> b -> s a -> Maybe a
  match' w x w' = case eqS w w' of
    Nothing -> Nothing
    Just Refl -> Just x

-- | A value of some type in data family @s@ wrapped in an @f@, together with its witness.
data AnyF s f where
  AnyF :: s ix -> f ix -> AnyF s f

-- | Helper constructor.
mkAnyF :: El s ix => r ix -> AnyF s r
mkAnyF = AnyF proof

-- | Unify an 'AnyF' with an @f a@.
matchAnyF :: forall s f a. EqS s => s a -> AnyF s f -> Maybe (f a)
matchAnyF p (AnyF w x) = match' w x p where
  match' :: forall b. s b -> f b -> s a -> Maybe (f a)
  match' w x w' = case eqS w w' of
    Nothing -> Nothing
    Just Refl -> Just x

-- | Unwrap an 'AnyF' and pass it to a function.
($?) :: (forall b. s b -> f b -> a) -> AnyF s f -> a
f $? (AnyF p x) = f p x

-- | Removes the value from its functor @f@.
unwrapAnyF :: (forall ix. f ix -> ix) -> AnyF s f -> Any s
unwrapAnyF g (AnyF ix rix) = Any ix (g rix)
