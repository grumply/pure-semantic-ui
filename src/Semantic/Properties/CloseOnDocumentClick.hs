module Semantic.Properties.CloseOnDocumentClick where

import Semantic.Properties.Utils

class HasCloseOnDocumentClickProp a where
    getCloseOnDocumentClick :: a -> Bool
    setCloseOnDocumentClick :: Bool -> a -> a

pattern CloseOnDocumentClick :: HasCloseOnDocumentClickProp a => a -> a
pattern CloseOnDocumentClick a <- (getCloseOnDocumentClick &&& id -> (True,a)) where
    CloseOnDocumentClick a = setCloseOnDocumentClick True a