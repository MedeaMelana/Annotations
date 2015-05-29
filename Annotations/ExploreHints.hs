module Annotations.ExploreHints (ExploreHints(..)) where

-- | Captures hints for the exploration of annotated trees.
data ExploreHints = ExploreHints
  { matchHere     :: Bool  -- ^ Whether the current focus matches.
  , exploreDown   :: Bool  -- ^ Whether to explore the children.
  , exploreRight  :: Bool  -- ^ Whether to explore further to the right.
  }
