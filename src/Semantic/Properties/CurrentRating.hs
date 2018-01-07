module Semantic.Properties.CurrentRating where

import Semantic.Properties.Utils

class HasCurrentRatingProp a where
    getCurrentRating :: a -> Maybe Int
    setCurrentRating :: Maybe Int -> a -> a

pattern CurrentRating :: HasCurrentRatingProp a => Maybe Int -> a -> a
pattern CurrentRating n a <- (getCurrentRating &&& id -> (n,a)) where
    CurrentRating n a = setCurrentRating n a