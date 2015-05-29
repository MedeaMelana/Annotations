module Annotations.F.ParserCombinators (
    -- * Parser combinators for bounds parsers
    mkBounded, unit, chainr, chainl
  ) where

import Annotations.Bounds
import Annotations.BoundsParser
import Annotations.F.Annotated

import qualified Text.Parsec as P

import Data.Function


-- | Given the left margin of a structure, asks the parser for the right
--   margin and wraps the position information around the root of the tree.
mkBounded :: Monad m => Range -> AnnFix1 Bounds f -> P s m (AnnFix Bounds f)
mkBounded left x = do -- (\right -> mkAnnFix (Bounds left right) x) <$> getPos
  right <- getPos
  return (mkAnnFix (Bounds left right) x)

-- | Wrap an unnotated tree with position information from the parse state.
unit :: Monad m => P s m (AnnFix1 Bounds f) -> P s m (AnnFix Bounds f)
unit p = do
  left <- getPos
  x <- p
  mkBounded left x

-- | Parse right-recursive structures.
chainr :: Monad m => P s m (AnnFix Bounds f) ->
  P s m (AnnFix Bounds f -> AnnFix Bounds f -> AnnFix1 Bounds f) -> P s m (AnnFix Bounds f)
chainr px pf = fix $ \loop -> do
  left <- getPos
  x <- px
  P.option x $ do
    f <- pf
    y <- loop
    mkBounded left (f x y)

-- | Parse left-recursive structures.
chainl :: Monad m => P s m (AnnFix Bounds f) ->
  P s m (AnnFix Bounds f -> AnnFix Bounds f -> AnnFix1 Bounds f) -> P s m (AnnFix Bounds f)
chainl px pf = do
    left <- getPos
    px >>= rest left
  where
    rest left = fix $ \loop x -> P.option x $ do
      f <- pf
      y <- px
      mkBounded left (f x y) >>= loop
