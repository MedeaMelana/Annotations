-- | The 'Except' datatype captures monoidal exceptions in applicative 
-- computations.
module Annotations.Except where

import Data.Monoid
import Control.Applicative

-- | @Except@ is like @Either@ but is meant to be used only in applicative 
-- computations. When two exceptions are sequenced, their sum (using 
-- 'mappend') is computed.
data Except e a = Failed e | OK a
  deriving (Eq, Show)

instance Functor (Except e) where
  fmap f  (OK x) = OK (f x)
  fmap _  (Failed e) = Failed e

instance Monoid e => Applicative (Except e) where
  pure = OK
  OK f <*> OK x = OK (f x)
  OK _ <*> Failed e = Failed e
  Failed e <*> OK _ = Failed e
  Failed e1 <*> Failed e2 = Failed (e1 `mappend` e2)
