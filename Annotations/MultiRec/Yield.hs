{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Annotations.MultiRec.Yield
  ( MonadYield(..)
  , YieldT, Yield
  , runYield, runYieldG, runYieldT, runYieldTG
  ) where

import Control.Monad.State.Strict
import Control.Monad.Identity

import Generics.MultiRec hiding (show)
import Generics.MultiRec.HFix

import Annotations.MultiRec.Annotated
import Annotations.MultiRec.Any

-- | Monads that allow yielding recursively annotated values.
class Monad m => MonadYield m where
  -- | Yielded values have types in this datatype family.
  type YieldFam m :: * -> *
  
  -- | The type of the annotation.
  type AnnType  m :: *
  
  -- | Yields a value with its annotation.
  yield :: (YieldFam m) ix -> AnnType m -> ix -> m ix


-- | The Yield transformer. Allows yielding generic values in family @fam@ with annotations of type @x@.
newtype YieldT x fam m a = YieldT (StateT [AnyAnnFix x fam] m a)
  deriving (Functor, Applicative, Monad)

runYieldTG :: Monad m => YieldT x fam m a -> m (a, Maybe (AnyAnnFix x fam))
runYieldTG (YieldT y) = do
  (x, zs) <- runStateT y []
  return $ (,) x $ case zs of
    [z] -> Just z
    _   -> Nothing

runYieldT :: (Monad m, EqS fam) => fam a -> YieldT x fam m a -> m (Maybe (AnnFix x fam a))
runYieldT p y = do
  (_, x) <- runYieldTG y
  return (x >>= matchAnyF p)

runYieldG :: Yield x fam a -> (a, Maybe (AnyAnnFix x fam))
runYieldG = runIdentity . runYieldTG

runYield :: EqS fam => fam a -> Yield x fam a -> Maybe (AnnFix x fam a)
runYield p = runIdentity . runYieldT p

-- | Yield over the identity monad.
type Yield x fam = YieldT x fam Identity

instance MonadTrans (YieldT x fam) where
  lift = YieldT . lift


instance (Monad m, HFunctor fam (PF fam), EqS fam, Fam fam) => MonadYield (YieldT x fam m) where
  type YieldFam (YieldT x fam m) = fam
  type AnnType (YieldT x fam m) = x
  yield = doYield

doYield :: (Monad m, HFunctor fam (PF fam), EqS fam, Fam fam) =>
  fam ix -> x -> ix -> YieldT x fam m ix
doYield p bounds x = YieldT $ do
  let pfx = from p x
  let n = length (children p pfx)
  stack <- get
  if length stack < n
    then
      fail ("structure mismatch: required " ++ show n ++
            " accumulated children but found only " ++
            show (length stack))
    else do
      let (cs, cs') = splitAt n stack
      let newChild = evalState (hmapM distribute p pfx) (reverse cs)
      put (AnyF p (HIn (K bounds :*: newChild)) : cs')
      return x

distribute :: EqS phi => phi ix -> I0 ix -> State [AnyAnnFix x phi] (AnnFix x phi ix)
distribute w _ = do
  xs <- get
  case xs of
    [] -> error "structure mismatch: too few children"
    AnyF w' x : xs' ->
      case eqS w w' of
        Nothing   -> error "structure mismatch: incompatible child type"
        Just Refl -> do put xs'; return x
