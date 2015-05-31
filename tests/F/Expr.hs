module F.Expr where

import Annotations.Bounds
import Annotations.F.Annotated
import Annotations.F.Fixpoints

import Control.Applicative (Applicative(..), (<$>))
import Data.Foldable (Foldable(..))
import Data.Traversable


-- type  PositionalExpr = (Bounds, PositionalExpr')
-- data  PositionalExpr'
--   =  Add  PositionalExpr  PositionalExpr
--   |  Sub  PositionalExpr  PositionalExpr
--   |  Mul  PositionalExpr  PositionalExpr
--   |  Div  PositionalExpr  PositionalExpr
--   |  Num  Int

data ExprF rT
  =  Add  rT  rT
  |  Sub  rT  rT
  |  Mul  rT  rT
  |  Div  rT  rT
  |  Num  Int
  deriving (Eq, Show)

instance Functor ExprF where
  fmap = fmapDefault

instance Foldable ExprF where
  foldMap = foldMapDefault

instance Traversable ExprF where
  traverse f expr = case expr of
    Add x y -> Add <$> f x <*> f y
    Sub x y -> Sub <$> f x <*> f y
    Mul x y -> Mul <$> f x <*> f y
    Div x y -> Div <$> f x <*> f y
    Num n   -> pure (Num n)

-- newtype Expr = Expr (ExprF Expr)
-- data PositionalExpr = PositionalExpr Bounds (ExprF PositionalExpr)

newtype Expr    = Expr  { runExpr  :: Fix ExprF    }
  deriving (Eq, Show)

instance Num Expr where
  fromInteger = Expr . In . Num . fromIntegral
  Expr x + Expr y = Expr $ In $ Add x y
  Expr x - Expr y = Expr $ In $ Sub x y
  Expr x * Expr y = Expr $ In $ Mul x y
  negate = (0 -)
  abs = error "abs"
  signum = error "signum"

instance Fractional Expr where
  Expr x / Expr y = Expr $ In $ Div x y
  fromRational = error "fromRational"

type PositionalExpr = Fix (Ann Bounds ExprF)

cataExpr :: (ExprF a -> a) -> Fix ExprF -> a
cataExpr f (In expr) = f (fmap (cataExpr f) expr)

data ExprAlg a = ExprAlg
  { cataNum :: Int -> a
  , cataAdd :: a -> a -> a
  , cataSub :: a -> a -> a
  , cataMul :: a -> a -> a
  , cataDiv :: a -> a -> a
  }

cataExpr0 :: ExprAlg a -> Fix ExprF -> a
cataExpr0 alg = f where
  f (In expr) = case expr of
    Num n    -> cataNum  alg n
    Add x y  -> cataAdd  alg (f x) (f y)
    Sub x y  -> cataSub  alg (f x) (f y)
    Mul x y  -> cataMul  alg (f x) (f y)
    Div x y  -> cataDiv  alg (f x) (f y)

exprEval :: Algebra ExprF Int
exprEval expr = case expr of
  Num  n    -> n
  Add  x y  -> x + y
  Sub  x y  -> x - y
  Mul  x y  -> x * y
  Div  x y  -> x `div` y

exprEval' :: Algebra (Ann Bounds ExprF)
  (Either (Bounds, String) Int)
exprEval' (Ann z expr) = case expr of
    Num n     -> Right n
    Add  x y  -> (+)  <$> x <*> y
    Sub  x y  -> (-)  <$> x <*> y
    Mul  x y  -> (*)  <$> x <*> y
    Div  x y  -> do
      x' <- x
      y' <- y
      if y' == 0
        then Left   (z, "division by zero")
        else Right  (x' `div` y')

exprEvalE :: ErrorAlgebra ExprF String Int
exprEvalE expr = case expr of
  Num  n         -> Right n
  Add  x y       -> Right (x +  y)
  Sub  x y       -> Right (x -  y)
  Mul  x y       -> Right (x *  y)
  Div  x y
    | y == 0     -> Left "division by zero"
    | otherwise  -> Right (x `div` y)
