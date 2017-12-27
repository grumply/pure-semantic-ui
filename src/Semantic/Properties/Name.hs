module Semantic.Properties.Name where

import Semantic.Properties.Utils

class HasNameProp a where
    getName :: a -> Txt
    setName :: Txt -> a -> a

pattern Name :: HasNameProp a => Txt -> a -> a
pattern Name n a <- (getName &&& id -> (n,a)) where
    Name n a = setName n a

