module Semantic.Properties.Widths where

import Semantic.Properties.Utils

import Semantic.Utils

class HasWidthsProp a where
    getWidths :: a -> Width
    setWidths :: Width -> a -> a

pattern Widths :: HasWidthsProp a => Width -> a -> a
pattern Widths w a <- (getWidths &&& id -> (w,a)) where
    Widths w a = setWidths w a