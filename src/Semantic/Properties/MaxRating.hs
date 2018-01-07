module Semantic.Properties.MaxRating where

import Semantic.Properties.Utils

class HasMaxRatingProp a where
    getMaxRating :: a -> Int
    setMaxRating :: Int -> a -> a

pattern MaxRating :: HasMaxRatingProp a => Int -> a -> a
pattern MaxRating n a <- (getMaxRating &&& id -> (n,a)) where
    MaxRating n a = setMaxRating n a