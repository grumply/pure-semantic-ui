module Semantic.Extensions.Name where

import Semantic.Extensions.Utils

class HasName a where
    getName :: a -> Txt
    setName :: Txt -> a -> a

pattern Name :: HasName a => Txt -> a -> a
pattern Name n a <- (getName &&& id -> (n,a)) where
    Name n a = setName n a

