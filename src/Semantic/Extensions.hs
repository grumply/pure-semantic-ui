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

pattern Pointing t c <- (getPointing -> Just (t,c)) where
    Pointing t c = setPointing t c

{-# INLINE getPointing #-}
getPointing c =
    case c of
        View Label_ {..} -> may (\p -> Just (p,c)) pointing
        _                -> Nothing

{-# INLINE setPointing #-}
setPointing t c =
    case c of
        View Label_ {..} -> View Label_ { pointing = Just t, .. }
        _                -> c

pattern Position p c <- (getPosition -> Just (p,c)) where
    Position p c = setPosition p c

{-# INLINE getPosition #-}
getPosition c =
    case c of
        View Rail_ {..} -> Just (position,c)
        _               -> Nothing

{-# INLINE setPosition #-}
setPosition p c =
    case c of
        View Rail_ {..} -> View Rail_ { position = p, .. }
        _               -> c

pattern Positive c <- (getPositive -> (True,c)) where
    Positive c = setPositive c

{-# INLINE getPositive #-}
getPositive c =
    case c of
        View Button_      {..} -> (positive,c)
        View ButtonGroup_ {..} -> (positive,c)
        _                      -> (False,c)

{-# INLINE setPositive #-}
setPositive c =
    case c of
        View Button_      {..} -> View Button_      { positive = True, .. }
        View ButtonGroup_ {..} -> View ButtonGroup_ { positive = True, .. }
        _                      -> c

pattern Primary c <- (getPrimary -> (True,c)) where
    Primary c = setPrimary c

{-# INLINE getPrimary #-}
getPrimary c =
    case c of
        View Button_      {..} -> (primary,c)
        View ButtonGroup_ {..} -> (primary,c)
        _                      -> (False,c)

{-# INLINE setPrimary #-}
setPrimary c =
    case c of
        View Button_      {..} -> View Button_      { primary = True, .. }
        View ButtonGroup_ {..} -> View ButtonGroup_ { primary = True, .. }
        _                      -> c

pattern Relaxed r c <- (getRelaxed -> (Just r,c)) where
    Relaxed r c = setRelaxed r c

{-# INLINE getRelaxed #-}
getRelaxed c =
    case c of
        View List_ {..} -> (relaxed,c)
        _               -> (Nothing,c)

{-# INLINE setRelaxed #-}
setRelaxed r c =
    case c of
        View List_ {..} -> View List_ { relaxed = Just r, .. }
        _               -> c

pattern Ribbon r c <- (getRibbon -> (Just r,c)) where
    Ribbon r c = setRibbon r c

{-# INLINE getRibbon #-}
getRibbon c =
    case c of
        View Label_ {..} -> (ribbon,c)
        _                -> (Nothing,c)

{-# INLINE setRibbon #-}
setRibbon r c =
    case c of
        View Label_ {..} -> View Label_ { ribbon = Just r, .. }
        _                -> c

pattern Rotated r c <- (getRotated -> Just (r,c)) where
    Rotated r c = setRotated r c

{-# INLINE getRotated #-}
getRotated c =
    case c of
        View Icon_     {..} -> rotated # Just (rotated,c)
        View ListIcon_ {..} -> rotated # Just (rotated,c)
        _                   -> Nothing

{-# INLINE setRotated #-}
setRotated r c =
    case c of
        View Icon_     {..} -> View Icon_     { rotated = r, .. }
        View ListIcon_ {..} -> View ListIcon_ { rotated = r, .. }
        _                   -> c

pattern Rounded c <- (getRounded -> (True,c)) where
    Rounded c = setRounded c

{-# INLINE getRounded #-}
getRounded c =
    case c of
        View Image_ {..} -> (rounded,c)
        _                -> (False,c)

{-# INLINE setRounded #-}
setRounded c =
    case c of
        View Image_ {..} -> View Image_ { rounded = True, .. }
        _                -> c

pattern Secondary c <- (getSecondary -> (True,c)) where
    Secondary c = setSecondary c

{-# INLINE getSecondary #-}
getSecondary c =
    case c of
        View Button_      {..} -> (secondary,c)
        View ButtonGroup_ {..} -> (secondary,c)
        _                      -> (False,c)

{-# INLINE setSecondary #-}
setSecondary c =
    case c of
        View Button_      {..} -> View Button_      { secondary = True, .. }
        View ButtonGroup_ {..} -> View ButtonGroup_ { secondary = True, .. }
        _                 -> c

pattern Section c <- (getSection -> (True,c)) where
    Section c = setSection c

{-# INLINE getSection #-}
getSection c =
    case c of
        View Divider_ {..} -> (section,c)
        _                  -> (False,c)

{-# INLINE setSection #-}
setSection c =
    case c of
        View Divider_ {..} -> View Divider_ { section = True, .. }
        _                  -> c

pattern Selection c <- (getSelection -> (True,c)) where
    Selection c = setSelection c

{-# INLINE getSelection #-}
getSelection c =
    case c of
        View List_ {..} -> (selection,c)
        _               -> (False,c)

{-# INLINE setSelection #-}
setSelection c =
    case c of
        View List_ {..} -> View List_ { selection = True, .. }
        _               -> c

pattern Size s c <- (getSize -> Just (s,c)) where
    Size s c = setSize s c

pattern Mini c = Size "mini" c
pattern Tiny c = Size "tiny" c
pattern Small c = Size "small" c
pattern Medium c = Size "medium" c
pattern Large c = Size "large" c
pattern Big c = Size "big" c
pattern Huge c = Size "huge" c
pattern Massive c = Size "massive" c

{-# INLINE getSize #-}
getSize c =
    case c of
        View Button_      {..} -> size # Just (size,c)
        View ButtonGroup_ {..} -> size # Just (size,c)
        View Header_      {..} -> size # Just (size,c)
        View Icon_        {..} -> size # Just (size,c)
        View IconGroup_   {..} -> size # Just (size,c)
        View Image_       {..} -> size # Just (size,c)
        View ImageGroup_  {..} -> size # Just (size,c)
        View Input_       {..} -> size # Just (size,c)
        View Label_       {..} -> size # Just (size,c)
        View LabelGroup_  {..} -> size # Just (size,c)
        View List_        {..} -> size # Just (size,c)
        View ListIcon_    {..} -> size # Just (size,c)
        View Loader_      {..} -> size # Just (size,c)
        View Rail_        {..} -> size # Just (size,c)
        _                      -> Nothing

{-# INLINE setSize #-}
setSize s c =
    case c of
        View Button_      {..} -> View Button_      { size = s, .. }
        View ButtonGroup_ {..} -> View ButtonGroup_ { size = s, .. }
        View Header_      {..} -> View Header_      { size = s, .. }
        View Icon_        {..} -> View Icon_        { size = s, .. }
        View IconGroup_   {..} -> View IconGroup_   { size = s, .. }
        View Image_       {..} -> View Image_       { size = s, .. }
        View ImageGroup_  {..} -> View ImageGroup_  { size = s, .. }
        View Input_       {..} -> View Input_       { size = s, .. }
        View Label_       {..} -> View Label_       { size = s, .. }
        View LabelGroup_  {..} -> View LabelGroup_  { size = s, .. }
        View List_        {..} -> View List_        { size = s, .. }
        View ListIcon_    {..} -> View ListIcon_    { size = s, .. }
        View Loader_      {..} -> View Loader_      { size = s, .. }
        View Rail_        {..} -> View Rail_        { size = s, .. }
        _                      -> c

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

