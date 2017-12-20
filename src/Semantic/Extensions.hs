module Semantic.Extensions where

import Pure.View (pattern View)

import qualified Semantic.Elements.Container as Container

import Semantic.Utils

pattern TextAlign ta c <- (getTextAlign -> Just (Just ta,c)) where
    TextAlign ta c = setTextAlign ta c

getTextAlign c =
    case c of
        View Container.Container_ {..} -> Just (textAlign,c)

setTextAlign ta c =
    case c of
        View Container.Container_ {..} -> View Container.Container_ { textAlign = Just ta, .. }

pattern TextAlignLeft c = TextAlign AlignedLeft c
pattern TextAlignCenter c = TextAlign AlignedCenter c
pattern TextAlignRight c = TextAlign AlignedRight c
pattern TextAlignJustified c = TextAlign AlignedJustified c