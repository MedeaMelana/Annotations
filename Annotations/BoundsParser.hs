-- | A Parsec parser type that parses 'Symbol's and keeps track of the 
-- position within the input stream. Unlike Parsec's default position 
-- tracking, this parser keeps track of the range of whitespace between two 
-- tokens.
module Annotations.BoundsParser
  ( -- * Symbols
    Symbol(..), collapse,
    
    -- * Parsing
    P, satisfy, pToken, getPos
  ) where

import Annotations.Bounds

import qualified Text.Parsec as P
import qualified Text.Parsec.Pos as P


-- | Symbols form input for parsers. Minimal complete definition: 'unparse'.
class Symbol s where
  -- | Unparses a symbol, converting it back to text.
  unparse :: s -> String

  -- | Yields the size of a symbol. Default implementation is @length . unparse@.
  symbolSize :: s -> Int
  symbolSize = length . unparse

instance Symbol s => Symbol [s] where
  unparse = concatMap unparse
  symbolSize = sum . fmap symbolSize

-- | Given a predicate that tells what tokens to discard, keeps only the meaningful tokens and couples them with position information.
collapse :: Symbol s => (s -> Bool) -> [s] -> [(s, Bounds)]
collapse space ts = collapse' (0, symbolSize lefts) space rest
  where
    (lefts, rest) = span space ts

collapse' :: Symbol s => Range -> (s -> Bool) -> [s] -> [(s, Bounds)]
collapse' _ _ [] = []
collapse' left space (t:ts) = new : collapse' right space rest
  where
    (_, leftInner)  = left
    rightInner      = leftInner   + symbolSize t
    rightOuter      = rightInner  + symbolSize rights
    right           = (rightInner, rightOuter)
    (rights, rest)  = span space ts
    new             = (t, Bounds left right)



-- | A parser that works on symbols coupled with token information. The state maintains the current position in the stream. This position is the range of whitespace between two tokens.
type P s = P.ParsecT [(s, Bounds)] Range

-- | Yield the current position in the input.
getPos :: Monad m => P s m Range
getPos = P.getState

-- | Recognise a symbol matching a predicate.
satisfy :: (Monad m, Symbol s) => (s -> Bool) -> P s m s
satisfy ok = do
  let pos _ (_, bounds) _ = P.newPos "" 0 (fst (rightMargin bounds) + 1)
  let match x@(tok, _)
        | ok tok    = Just x
        | otherwise = Nothing
  (tok, bounds) <- P.tokenPrim (unparse . fst) pos match
  P.setState (rightMargin bounds)
  return tok

-- | Recognise a specific symbol.
pToken :: (Monad m, Symbol s, Eq s) => s -> P s m s
pToken = satisfy . (==)

