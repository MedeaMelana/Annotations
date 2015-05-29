-- | A Show-like type class for families of data types.
module Annotations.MultiRec.ShowFam where

class ShowFam fam where
  -- | Given a witness, convert the value of that type to String.
  showFam :: fam a -> a -> String
