{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}

module MultiRec.ExprExpl where

import Generics.MultiRec
import Control.Applicative

data Expr
  = EAdd     Expr  Expr
  | EMul     Expr  Expr
  | ETup     Expr  Expr
  | EIntLit  Int
  | ETyped   Expr  Type
  deriving (Eq, Show)

data Type
  = TyInt
  | TyTup  Type Type
  deriving (Eq, Show)

type PF_Expr =
       I  Expr  :*:  I  Expr  :*:  U
  :+:  I  Expr  :*:  I  Expr  :*:  U
  :+:  I  Expr  :*:  I  Expr  :*:  U
  :+:  K  Int   :*:  U
  :+:  I  Expr  :*:  I  Type  :*:  U

type PF_Type =
       U
  :+:  I  Type  :*:  I  Type  :*:  U

type PF_Tuples = PF_Expr :>: Expr :+: PF_Type :>: Type

data Tuples :: * -> * where
  Expr :: Tuples Expr
  Type :: Tuples Type

type instance PF Tuples = PF_Tuples

instance EqS Tuples where
  eqS Expr Expr = Just Refl
  eqS Type Type = Just Refl
  eqS _    _    = Nothing

instance El Tuples Expr where
  proof = Expr

instance El Tuples Type where
  proof = Type

instance Fam Tuples where

  from Expr ex = L . Tag $ case ex of
    EAdd x y     -> L              $ I (I0 x)  :*: I (I0 y)  :*: U
    EMul x y     -> R . L          $ I (I0 x)  :*: I (I0 y)  :*: U
    ETup x y     -> R . R . L      $ I (I0 x)  :*: I (I0 y)  :*: U
    EIntLit n    -> R . R . R . L  $ K n       :*: U
    ETyped e t   -> R . R . R . R  $ I (I0 e)  :*: I (I0 t)  :*: U
  from Type ty = R . Tag $ case ty of
      TyInt      -> L              $ U
      TyTup x y  -> R              $ I (I0 x)  :*: I (I0 y)  :*: U

  to Expr (L (Tag ex)) = case ex of
    L          (I (I0 x) :*: I (I0 y) :*: U)     -> EAdd x y
    R (L       (I (I0 x) :*: I (I0 y) :*: U))    -> EMul x y
    R (R (L    (I (I0 x) :*: I (I0 y) :*: U)))   -> ETup x y
    R (R (R (L (K n                   :*: U))))  -> EIntLit n
    R (R (R (R (I (I0 x) :*: I (I0 y) :*: U))))  -> ETyped x y
  to Type (R (Tag ty)) = case ty of
    L          U                                 -> TyInt
    R          (I (I0 x) :*: I (I0 y) :*: U)     -> TyTup x y


type family ErrorAlg (f :: (* -> *) -> * -> *) e ix :: *

type instance ErrorAlg U              e a = Either e a
type instance ErrorAlg (K b   :*: f)  e a = b  -> ErrorAlg f e a
type instance ErrorAlg (I xi  :*: f)  e a = a  -> ErrorAlg f e a
type instance ErrorAlg (f :+: g)      e a = (ErrorAlg f e a, ErrorAlg g e a)
type instance ErrorAlg (f :>: xi)     e a = ErrorAlg f e a

class MkErrorAlg f where
  mkErrorAlg :: ErrorAlg f e a -> f (K0 a) ix -> Either e a

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
