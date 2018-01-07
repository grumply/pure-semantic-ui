module Semantic.Properties.DefaultRating where

import Semantic.Properties.Utils

class HasDefaultRatingProp a where
    getDefaultRating :: a -> Maybe Int
    setDefaultRating :: Maybe Int -> a -> a

pattern DefaultRating :: HasDefaultRatingProp a => Maybe Int -> a -> a
pattern DefaultRating n a <- (getDefaultRating &&& id -> (n,a)) where
    DefaultRating n a = setDefaultRating n a
