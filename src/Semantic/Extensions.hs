module Semantic.Extensions where

import Pure.Data
import Pure.View (VC,Feature,View,pattern View)

import Semantic.Elements.Button
import Semantic.Elements.Container
import Semantic.Elements.Divider
import Semantic.Elements.Header
import Semantic.Elements.Label
import Semantic.Elements.Icon
import Semantic.Elements.Image
import Semantic.Elements.Input
import Semantic.Elements.List
import Semantic.Elements.Loader
import Semantic.Elements.Rail

import Semantic.Utils

import Debug.Trace

pattern Value v c <- (getValue -> Just (v,c)) where
    Value c = setValue c

{-# INLINE getValue #-}
getValue c =
    case c of
        View ListItem_ {..} -> value # Just (value,c)
        _                   -> Nothing

{-# INLINE setValue #-}
setValue v c =
    case c of
        View ListItem_ {..} -> View ListItem_ { value = v, .. }
        _                   -> c

pattern Vertical c <- (getVertical -> (True,c)) where
    Vertical c = setVertical c

{-# INLINE getVertical #-}
getVertical c =
    case c of
        View ButtonGroup_ {..} -> (vertical,c)
        View Divider_     {..} -> (vertical,c)
        _                      -> (False,c)

{-# INLINE setVertical #-}
setVertical c =
    case c of
        View ButtonGroup_ {..} -> View ButtonGroup_ { vertical = True, .. }
        View Divider_     {..} -> View Divider_     { vertical = True, .. }
        _                      -> c

pattern VerticalAlign va c <- (getVerticalAlign -> Just (va,c)) where
    VerticalAlign va c = setVerticalAlign va c

pattern Middle = "middle"
-- pattern Bottom = "bottom"
-- pattern Top = "top"

{-# INLINE getVerticalAlign #-}
getVerticalAlign c =
    case c of
        View Image_       {..} -> Just (verticalAlign,c)
        View List_        {..} -> Just (verticalAlign,c)
        View ListContent_ {..} -> Just (verticalAlign,c)
        View ListIcon_    {..} -> Just (verticalAlign,c)
        _                      -> Nothing

{-# INLINE setVerticalAlign #-}
setVerticalAlign va c =
    case c of
        View Image_        {..} -> View Image_        { verticalAlign = va, .. }
        View List_         {..} -> View List_         { verticalAlign = va, .. }
        View ListContent_  {..} -> View ListContent_  { verticalAlign = va, .. }
        View ListIcon_     {..} -> View ListIcon_     { verticalAlign = va, .. }
        _                       -> c

pattern Visible c <- (getVisible -> (True,c)) where
    Visible c = setVisible c

{-# INLINE getVisible #-}
getVisible c =
    case c of
        View ButtonContent_ {..} -> (visible,c)
        _                        -> (False,c)

{-# INLINE setVisible #-}
setVisible c =
    case c of
        View ButtonContent_ {..} -> View ButtonContent_ { visible = True, .. }
        _                        -> c

-- pattern Visibility v c <- (getVisibility -> Just (v,c)) where
--     Visibility v c = setVisibility v c

pattern Mobile = "mobile"
pattern Tablet = "tablet"
pattern Computer = "computer"
pattern LargeScreen = "large screen"
pattern Widescreen = "widescreen"

-- {-# INLINE getVisibility #-}
-- getVisibility c =
--     case c of
--         _ -> Nothing

-- {-# INLINE setVisibility #-}
-- setVisibility c =
--     case c of
--         _ -> c

pattern Widths w c <- (getWidths -> Just (w,c)) where
    Widths w c = setWidths w c

{-# INLINE getWidths #-}
getWidths c =
    case c of
        View ButtonGroup_ {..} -> Just (widths,c)
        _                      -> Nothing

{-# INLINE setWidths #-}
setWidths w c =
    case c of
        View ButtonGroup_ {..} -> View ButtonGroup_ { widths = w, .. }
        _                      -> c

pattern Wrapped c <- (getWrapped -> (True,c)) where
    Wrapped c = setWrapped c

{-# INLINE getWrapped #-}
getWrapped c =
    case c of
        View Image_ {..} -> (wrapped,c)
        _                -> (False,c)

{-# INLINE setWrapped #-}
setWrapped c =
    case c of
        View Image_ {..} -> View Image_ { wrapped = True, .. }
        _                -> c

-- pattern Animate = "animate"
-- pattern Fade = "fade"
-- pattern Vertical = "vertical"

----------------------------------

