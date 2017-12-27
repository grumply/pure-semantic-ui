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

-- pattern ToLeft = "left"
-- pattern ToRight = "right"

pattern Spaced s c <- (getSpaced -> (Just s,c)) where
    Spaced s c = setSpaced s c

{-# INLINE getSpaced #-}
getSpaced c =
    case c of
        View Image_ {..} -> (spaced,c)
        _                -> (Nothing,c)

{-# INLINE setSpaced #-}
setSpaced s c =
    case c of
        View Image_ {..} -> View Image_ { spaced = Just s, .. }
        _                -> c

pattern Sub c <- (getSub -> (True,c)) where
    Sub c = setSub c

{-# INLINE getSub #-}
getSub c =
    case c of
        View Header_ {..} -> (sub,c)
        _                 -> (False,c)

{-# INLINE setSub #-}
setSub c =
    case c of
        View Header_ {..} -> View Header_ { sub = True, .. }
        _                 -> c

pattern TabIndex n c <- (getTabIndex -> Just (n,c)) where
  TabIndex n c = setTabIndex n c

{-# INLINE getTabIndex #-}
getTabIndex c =
    case c of
        View Button_ {..} -> Just (tabIndex,c)
        View Input_  {..} -> Just (tabIndex,c)
        _                 -> Nothing

{-# INLINE setTabIndex #-}
setTabIndex n c =
    case c of
        View Button_ {..} -> View Button_ { tabIndex = n, .. }
        View Input_  {..} -> View Input_  { tabIndex = n, .. }
        _                 -> c

pattern Tag c <- (getTag -> (True,c)) where
    Tag c = setTag c

{-# INLINE getTag #-}
getTag c =
    case c of
        View Label_ {..} -> (tag,c)
        View LabelGroup_ {..} -> (tag,c)
        _                -> (False,c)

{-# INLINE setTag #-}
setTag c =
    case c of
        View Label_ {..} -> View Label_ { tag = True, .. }
        View LabelGroup_ {..} -> View LabelGroup_ { tag = True, .. }
        _                -> c

pattern TextAlign ta c <- (getTextAlign -> Just (ta,c)) where
    TextAlign ta c = setTextAlign ta c

pattern Unaligned c = TextAlign "" c
pattern LeftAligned c = TextAlign "left aligned" c
pattern RightAligned c = TextAlign "right aligned" c
pattern CenterAligned c = TextAlign "center aligned" c
pattern Justified c = TextAlign "justified" c

{-# INLINE getTextAlign #-}
getTextAlign c =
    case c of
        View Container_ {..} -> Just (textAlign,c)
        View Header_    {..} -> Just (textAlign,c)
        _                    -> Nothing

{-# INLINE setTextAlign #-}
setTextAlign ta c =
    case c of
        View Container_ {..} -> View Container_ { textAlign = ta, .. }
        View Header_    {..} -> View Header_    { textAlign = ta, .. }
        _                    -> c

pattern Toggle c <- (getToggle -> (True,c)) where
    Toggle c = setToggle c

{-# INLINE getToggle #-}
getToggle c =
    case c of
        View Button_      {..} -> (toggle,c)
        View ButtonGroup_ {..} -> (toggle,c)
        _                      -> (False,c)

{-# INLINE setToggle #-}
setToggle c =
    case c of
        View Button_      {..} -> View Button_      { toggle = True, .. }
        View ButtonGroup_ {..} -> View ButtonGroup_ { toggle = True, .. }
        _                      -> c

-- pattern Transition t c <- (getTransition -> Just (t,c)) where
--     Transition t c = setTransition t c

pattern Scale = "scale"
pattern Fade = "fade"
pattern FadeUp = "fade up"
pattern FadeDown = "fade down"
pattern FadeLeft = "fade left"
pattern FadeRight = "fade right"
pattern HorizontalFlip = "horizontal flip"
pattern VerticalFlip = "vertical flip"
pattern Drop = "drop"
pattern FlyLeft = "fly left"
pattern FlyRight = "fly right"
pattern FlyUp = "fly up"
pattern FlyDown = "fly down"
pattern SwingLeft = "swing left"
pattern SwingRight = "swing right"
pattern SwingUp = "swing up"
pattern SwingDown = "swing down"
pattern Browse = "browse"
pattern BrowseRight = "browse right"
pattern SlideDown = "slide down"
pattern SlideUp = "slide up"
pattern SlideRight = "slide right"

pattern Jiggle = "jiggle"
pattern Flash = "flash"
pattern Shake = "shake"
pattern Pulse = "pulse"
pattern Tada = "tada"
pattern Bounce = "bounce"

-- {-# INLINE getTransition #-}
-- getTransition c =
--     case c of
--         _ -> Nothing

-- {-# INLINE setTransition #-}
-- setTransition t c =
--     case c of
--         _ -> c

pattern Transparent c <- (getTransparent -> (True,c)) where
    Transparent c = setTransparent c

{-# INLINE getTransparent #-}
getTransparent c =
    case c of
        View Input_ {..} -> (transparent,c)
        _                -> (False,c)

{-# INLINE setTransparent #-}
setTransparent c =
    case c of
        View Input_ {..} -> View Input_ { transparent = True, .. }
        _                -> c

pattern Type t c <- (getType -> Just (t,c)) where
    Type t c = setType t c

{-# INLINE getType #-}
getType c =
    case c of
        View Input_ {..} -> Just (_type,c)
        _                -> Nothing

{-# INLINE setType #-}
setType t c =
    case c of
        View Input_ {..} -> View Input_ { _type = t, .. }
        _                -> c

pattern UI c <- (getUI -> (True,c)) where
    UI c = setUI c

{-# INLINE getUI #-}
getUI c =
    case c of
        View Image_ {..} -> (ui,c)
        _                -> (False,c)

{-# INLINE setUI #-}
setUI c =
    case c of
        View Image_ {..} -> View Image_ { ui = True, .. }
        _                -> c

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

