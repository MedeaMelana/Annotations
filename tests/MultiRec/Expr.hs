{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types #-}

module MultiRec.Expr where

import Annotations.MultiRec.ShowFam
import Annotations.MultiRec.ErrorAlg

import Generics.MultiRec.Base


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

type instance PF Tuples = PF_Expr :>: Expr :+: PF_Type :>: Type

data Tuples :: * -> * where
  Expr  :: Tuples Expr
  Type  :: Tuples Type

instance EqS Tuples where
  eqS Expr Expr = Just Refl
  eqS Type Type = Just Refl
  eqS _    _    = Nothing

instance El Tuples Expr where
  proof = Expr

instance El Tuples Type where
  proof = Type

instance ShowFam Tuples where
  showFam Expr  = show
  showFam Type  = show

instance Fam Tuples where

  from Expr ex  = L . Tag $ case ex of
    EAdd x y     -> L              $ I (I0 x)  :*: I (I0 y)  :*: U
    EMul x y     -> R . L          $ I (I0 x)  :*: I (I0 y)  :*: U
    ETup x y     -> R . R . L      $ I (I0 x)  :*: I (I0 y)  :*: U
    EIntLit n    -> R . R . R . L  $ K n       :*: U
    ETyped e t   -> R . R . R . R  $ I (I0 e)  :*: I (I0 t)  :*: U
  from Type ty  = R . Tag $ case ty of
      TyInt      -> L              $ U
      TyTup x y  -> R              $ I (I0 x)  :*: I (I0 y)  :*: U

  to Expr (L (Tag ex))  = case ex of
    L           (I (I0 x) :*: I (I0 y) :*: U)     -> EAdd x y
    R (L        (I (I0 x) :*: I (I0 y) :*: U))    -> EMul x y
    R (R (L     (I (I0 x) :*: I (I0 y) :*: U)))   -> ETup x y
    R (R (R (L  (K n                   :*: U))))  -> EIntLit n
    R (R (R (R  (I (I0 x) :*: I (I0 y) :*: U))))  -> ETyped x y
  to Type (R (Tag ty))  = case ty of
    L           U                                 -> TyInt
    R           (I (I0 x) :*: I (I0 y) :*: U)     -> TyTup x y

-- $(deriveConstructors [''Expr, ''Type])
-- $(deriveFamily ''AST [''Expr, ''Type] "PFAST")
-- type instance PF AST = PFAST

-- inferTypeExpr :: ErrorAlg (PF_Expr :>: Expr) String Type
-- inferTypeExpr = undefined & undefined & undefined & undefined & undefined
-- 
-- inferTypeType :: ErrorAlg (PF_Type :>: Type) String Type
-- inferTypeType = undefined & undefined
-- 
-- inferType :: ErrorAlg (PF_Tuples) String Type
-- inferType = inferTypeExpr & inferTypeType

-- inferType :: ErrorAlg (PF Tuples) String Type

type ExprErrorAlg e a
   =   (a     -> a     ->  Either e a)
  :&:  (a     -> a     ->  Either e a)
  :&:  (a     -> a     ->  Either e a)
  :&:  (Int            ->  Either e a)
  :&:  (a     -> a     ->  Either e a)

type TypeErrorAlg e a
   =                       Either e a
  :&:  (a     -> a     ->  Either e a)

-- inferType :: ExprErrorAlg String Type :&: TypeErrorAlg String Type
inferType :: ErrorAlg (PF Tuples) String Type
inferType = ( equal "+" & equal "*" & tup & const (Right TyInt) & equal "::" )
          & ( Right TyInt & tup )
  where
    equal op ty1 ty2
      | ty1 == ty2 = Right ty1
      | otherwise  = Left ("lhs and rhs of " ++ op ++ " must have equal types")
    tup ty1 ty2 = Right (TyTup ty1 ty2)

type (:&:) = (,)
infixr :&:
