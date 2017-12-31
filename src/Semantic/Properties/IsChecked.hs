module Semantic.Properties.IsChecked where

import Semantic.Properties.Utils

data Checked = Unchecked | Indeterminate | Checked
  deriving (Eq,Ord,Enum,Bounded,Generic,Default)

class HasIsCheckedProp a where
    getIsChecked :: a -> Checked
    setIsChecked :: Checked -> a -> a

pattern IsChecked :: HasIsCheckedProp a => Checked -> a -> a
pattern IsChecked c a <- (getIsChecked &&& id -> (c,a)) where
    IsChecked c a = setIsChecked c a

