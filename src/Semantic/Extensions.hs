module Semantic.Extensions where

import Pure.View (pattern View)

import Semantic.Elements.Container as Container
import Semantic.Elements.Label as Label
import Semantic.Elements.Label.LabelDetail as Label
import Semantic.Elements.Label.LabelGroup as Label
import Semantic.Elements.Image as Image
import Semantic.Elements.Image.ImageGroup as Image

import Semantic.Utils

import Debug.Trace

pattern Children cs c <- (getChildren -> Just (cs,c)) where
    Children cs c = setChildren cs c

{-# INLINE getChildren #-}
getChildren c =
    case c of
        View Container_   {..} -> Just (children,c)
        View Label_       {..} -> Just (children,c)
        View LabelDetail_ {..} -> Just (children,c)
        View LabelGroup_  {..} -> Just (children,c)
        View Image_       {..} -> Just (children,c)
        View ImageGroup_  {..} -> Just (children,c)
        _                      -> Nothing

{-# INLINE setChildren #-}
setChildren cs c =
    case c of
        View Container_   {..} -> View Container_   { children = cs, .. }
        View Label_       {..} -> View Label_       { children = cs, .. }
        View LabelDetail_ {..} -> View LabelDetail_ { children = cs, .. }
        View LabelGroup_  {..} -> View LabelGroup_  { children = cs, .. }
        View Image_       {..} -> View Image_       { children = cs, .. }
        View ImageGroup_  {..} -> View ImageGroup_  { children = cs, .. }
        _                      -> c

pattern Attributes as c <- (getAttributes -> Just (as,c)) where
    Attributes as c = setAttributes as c

{-# INLINE getAttributes #-}
getAttributes c =
    case c of
        View Container_   {..} -> Just (attributes,c)
        View Label_       {..} -> Just (attributes,c)
        View LabelDetail_ {..} -> Just (attributes,c)
        View LabelGroup_  {..} -> Just (attributes,c)
        View Image_       {..} -> Just (attributes,c)
        View ImageGroup_  {..} -> Just (attributes,c)
        _                      -> Nothing

{-# INLINE setAttributes #-}
setAttributes cs c =
    case c of
        View Container_   {..} -> View Container_   { attributes = cs, .. }
        View Label_       {..} -> View Label_       { attributes = cs, .. }
        View LabelDetail_ {..} -> View LabelDetail_ { attributes = cs, .. }
        View LabelGroup_  {..} -> View LabelGroup_  { attributes = cs, .. }
        View Image_       {..} -> View Image_       { attributes = cs, .. }
        View ImageGroup_  {..} -> View ImageGroup_  { attributes = cs, .. }
        _                      -> c

pattern As as c <- (getAs -> Just (as,c)) where
    As as c = setAs as c

{-# INLINE getAs #-}
getAs c =
    case c of
        View Container_   {..} -> Just (as,c)
        View Label_       {..} -> Just (as,c)
        View LabelDetail_ {..} -> Just (as,c)
        View LabelGroup_  {..} -> Just (as,c)
        View Image_       {..} -> Just (as,c)
        View ImageGroup_  {..} -> Just (as,c)
        _                      -> Nothing

{-# INLINE setAs #-}
setAs a c =
    case c of
        View Container_   {..} -> View Container_   { as = a, .. }
        View Label_       {..} -> View Label_       { as = a, .. }
        View LabelDetail_ {..} -> View LabelDetail_ { as = a, .. }
        View LabelGroup_  {..} -> View LabelGroup_  { as = a, .. }
        View Image_       {..} -> View Image_       { as = a, .. }
        View ImageGroup_  {..} -> View ImageGroup_  { as = a, .. }
        _                      -> c

pattern TextAlign ta c <- (getTextAlign -> Just (Just ta,c)) where
    TextAlign ta c = setTextAlign ta c

{-# INLINE getTextAlign #-}
getTextAlign c =
    case c of
        View Container_  {..} -> Just (textAlign,c)

{-# INLINE setTextAlign #-}
setTextAlign ta c =
    case c of
        View Container_  {..} -> View Container_ { textAlign = Just ta, .. }

pattern TextAlignLeft c = TextAlign AlignedLeft c
pattern TextAlignCenter c = TextAlign AlignedCenter c
pattern TextAlignRight c = TextAlign AlignedRight c
pattern TextAlignJustified c = TextAlign AlignedJustified c


pattern Animate = "animate"
pattern Fade = "fade"
pattern Vertical = "vertical"
