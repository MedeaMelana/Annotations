{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE FlexibleInstances     #-}

module Annotations.MultiRec.ErrorAlg (ErrorAlg, ErrorAlg_PF, MkErrorAlg(..), errorCata, (&)) where

import Annotations.Except

import Generics.MultiRec.Base
import Generics.MultiRec.HFunctor
import Generics.MultiRec.HFix

import Control.Applicative

-- | An error algebra over pattern functors.
type ErrorAlg_PF f e a = forall ix. f (K0 a) ix -> Either e a

-- | Type family that converts pattern functors to convenient algebra types.
type family ErrorAlg (f :: (* -> *) -> * -> *) e a :: *

type instance ErrorAlg U              e a = Either e a
type instance ErrorAlg (K b   :*: f)  e a = b  -> ErrorAlg f e a
type instance ErrorAlg (I xi  :*: f)  e a = a  -> ErrorAlg f e a
type instance ErrorAlg (f :+: g)      e a = (ErrorAlg f e a, ErrorAlg g e a)
type instance ErrorAlg (f :>: xi)     e a = ErrorAlg f e a

-- | Converts convenient algebras to algebras that are able to work with
-- pattern functors.
class MkErrorAlg f where
  mkErrorAlg :: ErrorAlg f e a -> ErrorAlg_PF f e a

instance MkErrorAlg U where
  mkErrorAlg x U = x

instance MkErrorAlg f => MkErrorAlg (K a :*: f) where
  mkErrorAlg alg (K x :*: f) = mkErrorAlg (alg x) f

instance MkErrorAlg f => MkErrorAlg (I xi :*: f) where
  mkErrorAlg alg (I (K0 x) :*: f) = mkErrorAlg (alg x) f

instance MkErrorAlg f => MkErrorAlg (f :>: xi) where
  mkErrorAlg alg (Tag f) = mkErrorAlg alg f

instance (MkErrorAlg f, MkErrorAlg g) => MkErrorAlg (f :+: g) where
  mkErrorAlg (alg, _) (L x) = mkErrorAlg alg x
  mkErrorAlg (_, alg) (R y) = mkErrorAlg alg y

-- | Reduces a tree to a value according to the algebra, collecting potential
--   errors. The errors are combined with the annotations in the tree at the
--   positions at which the errors occurred.
errorCata :: HFunctor phi f => ErrorAlg_PF f e r ->
  phi ix -> HFix (K x :*: f) ix -> Except [(e, x)] r
errorCata alg p_f (HIn (K k :*: f)) =
    case hmapA (\p_g g -> K0 <$> errorCata alg p_g g) p_f f of
      Failed xs -> Failed xs
      OK expr' -> case alg expr' of
        Left x'  -> Failed [(x', k)]
        Right v  -> OK v

-- | For constructing algebras that are made of nested pairs rather
--   than n-ary tuples, it is helpful to use this pairing combinator.
(&) :: a -> b -> (a, b)
(&) = (,)

infixr 5 &
