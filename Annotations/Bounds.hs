-- | Types and utility functions for expression text selections.
module Annotations.Bounds
  ( -- * Types
    Range
  , Bounds(..), innerRange, outerRange
    -- * Membership predicates
  , posInRange, rangeInRange, rangeInBounds, rangesInBounds, distRange
  ) where


-- | A simple textual selection: starting offset and ending offset,
-- respectively. Offsets are 0-based.
type  Range         = (Int, Int)  -- left, right

-- | A structural selection expressed as a textual selection. The margins
-- indicate the whitespace directly around the selected structure.
data  Bounds        = Bounds { leftMargin :: Range, rightMargin :: Range }
  deriving (Eq, Show)

-- | A @Bounds@' inner range does not include the whitespace around the selected structure.
innerRange :: Bounds -> Range
innerRange (Bounds (_, left) (right, _)) = (left, right)

-- | A @Bounds@' outer range includes the whitespace around the selected structure.
outerRange :: Bounds -> Range
outerRange (Bounds (left, _) (_, right)) = (left, right)


-- | Tells whether the offset falls within the given range.
posInRange :: Int -> Range -> Bool
posInRange pos (left, right) = left <= pos && pos <= right

-- | Tells whether the first range is enclosed by the second range.
rangeInRange :: Range -> Range -> Bool
rangeInRange (left, right) range = left `posInRange` range && right `posInRange` range

-- | A range is within certain bounds if its left offset is within the bounds' 
-- left margin and its right offset is within the bounds' right margin.
rangeInBounds :: Range -> Bounds -> Bool
rangeInBounds (l, r) b =
  l `posInRange` leftMargin  b &&
  r `posInRange` rightMargin b

-- | @rangesInBounds b@ yields all those ranges @r@ for which
-- @rangeInBounds r b@.
rangesInBounds :: Bounds -> [Range]
rangesInBounds (Bounds (ol, il) (ir, or)) = [ (l, r) | l <- [ol..il], r <- [ir..or] ]

-- | A measure for the dissimilarity between two ranges.
-- 
--   @distRange (l1, r1) (l2, r2) = |l1 - l2| + |r1 - r2|@
distRange :: Range -> Range -> Int
distRange (l1, r1) (l2, r2) = abs (l1 - l2) + abs (r1 - r2)
