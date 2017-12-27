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

pattern Compact c <- (getCompact -> (True,c)) where
    Compact c = setCompact c

{-# INLINE getCompact #-}
getCompact c =
    case c of
        View Button_      {..} -> (compact,c)
        View ButtonGroup_ {..} -> (compact,c)
        _                      -> (False,c)

{-# INLINE setCompact #-}
setCompact c =
    case c of
        View Button_      {..} -> View Button_      { compact = True, .. }
        View ButtonGroup_ {..} -> View ButtonGroup_ { compact = True, .. }
        _                      -> c

pattern Disabled c <- (getDisabled -> (True,c)) where
    Disabled c = setDisabled c

{-# INLINE getDisabled #-}
getDisabled c =
    case c of
        View Button_   {..} -> (disabled,c)
        View Header_   {..} -> (disabled,c)
        View Icon_     {..} -> (disabled,c)
        View Image_    {..} -> (disabled,c)
        View Input_    {..} -> (disabled,c)
        View ListIcon_ {..} -> (disabled,c)
        View ListItem_ {..} -> (disabled,c)
        View Loader_   {..} -> (disabled,c)
        _                   -> (False,c)

{-# INLINE setDisabled #-}
setDisabled c =
    case c of
        View Button_   {..} -> View Button_   { disabled = True, .. }
        View Header_   {..} -> View Header_   { disabled = True, .. }
        View Icon_     {..} -> View Icon_     { disabled = True, .. }
        View Image_    {..} -> View Image_    { disabled = True, .. }
        View Input_    {..} -> View Input_    { disabled = True, .. }
        View ListIcon_ {..} -> View ListIcon_ { disabled = True, .. }
        View ListItem_ {..} -> View ListItem_ { disabled = True, .. }
        View Loader_   {..} -> View Loader_   { disabled = True, .. }
        _                   -> c

pattern Divided c <- (getDivided -> (True,c)) where
    Divided c = setDivided c

{-# INLINE getDivided #-}
getDivided c =
    case c of
        View List_ {..} -> (divided,c)
        _               -> (False,c)

{-# INLINE setDivided #-}
setDivided c =
    case c of
        View List_ {..} -> View List_ { divided = True, .. }
        _               -> c


pattern Dividing c <- (getDividing -> (True,c)) where
    Dividing c = setDividing c

{-# INLINE getDividing #-}
getDividing c =
    case c of
        View Header_ {..} -> (dividing,c)
        View Rail_   {..} -> (dividing,c)
        _                 -> (False,c)

{-# INLINE setDividing #-}
setDividing c =
    case c of
        View Header_ {..} -> View Header_ { dividing = True, .. }
        View Rail_   {..} -> View Rail_   { dividing = True, .. }
        _                 -> c

pattern Empty c <- (getEmpty -> (True,c)) where
    Empty c = setEmpty c

{-# INLINE getEmpty #-}
getEmpty c =
    case c of
        View Label_ {..} -> (empty,c)
        _                -> (False,c)

{-# INLINE setEmpty #-}
setEmpty c =
    case c of
        View Label_ {..} -> View Label_ { empty = True, .. }
        _                -> c

pattern Error c <- (getError -> (True,c)) where
    Error c = setError c

{-# INLINE getError #-}
getError c =
    case c of
        View Input_ {..} -> (error,c)
        _                -> (False,c)

{-# INLINE setError #-}
setError c =
    case c of
        View Input_ {..} -> View Input_ { error = True, .. }
        _                -> c

pattern Fitted c <- (getFitted -> (True,c)) where
    Fitted c = setFitted c

{-# INLINE getFitted #-}
getFitted c =
    case c of
        View Icon_     {..} -> (fitted,c)
        View Divider_  {..} -> (fitted,c)
        View ListIcon_ {..} -> (fitted,c)
        _                   -> (False,c)

{-# INLINE setFitted #-}
setFitted c =
    case c of
        View Icon_     {..} -> View Icon_     { fitted = True, .. }
        View Divider_  {..} -> View Divider_  { fitted = True, .. }
        View ListIcon_ {..} -> View ListIcon_ { fitted = True, .. }
        _                  -> c

pattern Flipped f c <- (getFlipped -> Just (f,c)) where
    Flipped f c = setFlipped f c

{-# INLINE getFlipped #-}
getFlipped c =
    case c of
        View Icon_     {..} -> flipped # Just (flipped,c)
        View ListIcon_ {..} -> flipped # Just (flipped,c)
        _               -> Nothing

{-# INLINE setFlipped #-}
setFlipped f c =
    case c of
        View Icon_     {..} -> View Icon_     { flipped = f, .. }
        View ListIcon_ {..} -> View ListIcon_ { flipped = f, .. }
        _               -> c

pattern Floated f c <- (getFloated -> Just (f,c)) where
    Floated f c = setFloated f c

{-# INLINE getFloated #-}
getFloated c =
    case c of
        View Button_      {..} -> floated # Just (floated,c)
        View ButtonGroup_ {..} -> floated # Just (floated,c)
        View Header_      {..} -> floated # Just (floated,c)
        View Image_       {..} -> floated # Just (floated,c)
        View List_        {..} -> floated # Just (floated,c)
        View ListContent_ {..} -> floated # Just (floated,c)
        _                      -> Nothing

{-# INLINE setFloated #-}
setFloated f c =
    case c of
        View Button_      {..} -> View Button_      { floated = f, .. }
        View ButtonGroup_ {..} -> View ButtonGroup_ { floated = f, .. }
        View Header_      {..} -> View Header_      { floated = f, .. }
        View Image_       {..} -> View Image_       { floated = f, .. }
        View List_        {..} -> View List_        { floated = f, .. }
        View ListContent_ {..} -> View ListContent_ { floated = f, .. }
        _                      -> c

-- pattern ToLeft = "left"
-- pattern ToRight = "right"

pattern Floating c <- (getFloating -> (True,c)) where
    Floating c = setFloating c

{-# INLINE getFloating #-}
getFloating c =
    case c of
        View Label_ {..} -> (floating,c)
        _                -> (False,c)

{-# INLINE setFloating #-}
setFloating c =
    case c of
        View Label_ {..} -> View Label_ { floating = True, .. }
        _                -> c

pattern Fluid c <- (getFluid -> (True,c)) where
    Fluid c = setFluid c

{-# INLINE getFluid #-}
getFluid c =
    case c of
        View Button_      {..} -> (fluid,c)
        View ButtonGroup_ {..} -> (fluid,c)
        View Container_   {..} -> (fluid,c)
        View Image_       {..} -> (fluid,c)
        View Input_       {..} -> (fluid,c)
        _                      -> (False,c)

{-# INLINE setFluid #-}
setFluid c =
    case c of
        View Button_      {..} -> View Button_      { fluid = True, .. }
        View ButtonGroup_ {..} -> View ButtonGroup_ { fluid = True, .. }
        View Container_   {..} -> View Container_   { fluid = True, .. }
        View Image_       {..} -> View Image_       { fluid = True, .. }
        View Input_       {..} -> View Input_       { fluid = True, .. }
        _                      -> c

pattern Focus c <- (getFocus -> (True,c)) where
    Focus c = setFocus c

{-# INLINE getFocus #-}
getFocus c =
    case c of
        View Button_ {..} -> (focus,c)
        View Input_  {..} -> (focus,c)
        _                 -> (False,c)

{-# INLINE setFocus #-}
setFocus c =
    case c of
        View Button_ {..} -> View Button_ { focus = True, .. }
        View Input_  {..} -> View Input_  { focus = True, .. }
        _                 -> c

pattern Focused c <- (getFocused -> (True,c)) where
    Focused c = setFocused c

{-# INLINE getFocused #-}
getFocused c =
    case c of
        View Input_ {..} -> (focused,c)
        _                -> (False,c)

{-# INLINE setFocused #-}
setFocused c =
    case c of
        View Input_ {..} -> View Input_ { focused = True, .. }
        _                -> c

pattern Hidden c <- (getHidden -> (True,c)) where
    Hidden c = setHidden c

{-# INLINE getHidden #-}
getHidden c =
    case c of
        View Divider_       {..} -> (hidden,c)
        View Image_         {..} -> (hidden,c)
        View ButtonContent_ {..} -> (hidden,c)
        _                        -> (False,c)

{-# INLINE setHidden #-}
setHidden c =
    case c of
        View Divider_       {..} -> View Divider_       { hidden = True, .. }
        View Image_         {..} -> View Image_         { hidden = True, .. }
        View ButtonContent_ {..} -> View ButtonContent_ { hidden = True, .. }
        _                        -> c

pattern Horizontal c <- (getHorizontal -> (True,c)) where
    Horizontal c = setHorizontal c

{-# INLINE getHorizontal #-}
getHorizontal c = 
    case c of
        View Divider_ {..} -> (horizontal,c)
        View Label_   {..} -> (horizontal,c)
        View List_    {..} -> (horizontal,c)
        _                  -> (False,c)

{-# INLINE setHorizontal #-}
setHorizontal c =
    case c of
        View Divider_ {..} -> View Divider_ { horizontal = True, .. }
        View Label_   {..} -> View Label_   { horizontal = True, .. }
        View List_    {..} -> View List_    { horizontal = True, .. }
        _                  -> c

pattern Indeterminate c <- (getIndeterminate -> (True,c)) where
    Indeterminate c = setIndeterminate c

{-# INLINE getIndeterminate #-}
getIndeterminate c =
    case c of
        View Loader_ {..} -> (indeterminate,c)
        _                 -> (False,c)

{-# INLINE setIndeterminate #-}
setIndeterminate c =
    case c of
        View Loader_ {..} -> View Loader_ { indeterminate = True, .. }
        _                 -> c

pattern Inline i c <- (getInline -> (Just i,c)) where
    Inline i c = setInline i c

{-# INLINE getInline #-}
getInline c =
    case c of
        View Image_  {..} -> inline # (Just "",c)
        View Loader_ {..} -> (inline,c)
        _                 -> (Nothing,c)

{-# INLINE setInline #-}
setInline i c =
    case c of
        View Image_  {..} -> View Image_  { inline = True, .. }
        View Loader_ {..} -> View Loader_ { inline = Just i, ..  }
        _                 -> c

pattern Internal c <- (getInternal -> (True,c)) where
    Internal c = setInternal c

{-# INLINE getInternal #-}
getInternal c =
    case c of
        View Rail_ {..} -> (internal,c)
        _               -> (False,c)

{-# INLINE setInternal #-}
setInternal c =
    case c of
        View Rail_ {..} -> View Rail_ { internal = True, .. }
        _               -> c

pattern Inverted c <- (getInverted -> (True,c)) where
    Inverted c = setInverted c

{-# INLINE getInverted #-}
getInverted c =
    case c of
        View Button_      {..} -> (inverted,c)
        View ButtonGroup_ {..} -> (inverted,c)
        View Divider_     {..} -> (inverted,c)
        View Header_      {..} -> (inverted,c)
        View Icon_        {..} -> (inverted,c)
        View Input_       {..} -> (inverted,c)
        View List_        {..} -> (inverted,c)
        View ListIcon_    {..} -> (inverted,c)
        View Loader_      {..} -> (inverted,c)
        _                      -> (False,c)

{-# INLINE setInverted #-}
setInverted c =
    case c of
        View Button_      {..} -> View Button_      { inverted = True, .. }
        View ButtonGroup_ {..} -> View ButtonGroup_ { inverted = True, .. }
        View Divider_     {..} -> View Divider_     { inverted = True, .. }
        View Header_      {..} -> View Header_      { inverted = True, .. }
        View Icon_        {..} -> View Icon_        { inverted = True, .. }
        View Input_       {..} -> View Input_       { inverted = True, .. }
        View List_        {..} -> View List_        { inverted = True, .. }
        View ListIcon_    {..} -> View ListIcon_    { inverted = True, .. }
        View Loader_      {..} -> View Loader_      { inverted = True, .. }
        _                      -> c

pattern Labeled c <- (getLabeled -> (True,c)) where
    Labeled c = setLabeled c

{-# INLINE getLabeled #-}
getLabeled c =
    case c of
        View ButtonGroup_ {..} -> (labeled,c)
        _                      -> (False,c)

{-# INLINE setLabeled #-}
setLabeled c =
    case c of
        View ButtonGroup_ {..} -> View ButtonGroup_ { labeled = True, .. }
        _                      -> c

pattern LabelPosition p c <- (getLabelPosition -> Just (p,c)) where
    LabelPosition p c = setLabelPosition p c

{-# INLINE getLabelPosition #-}
getLabelPosition c =
    case c of
        View Button_ {..} -> labelPosition # Just (labelPosition,c)
        _                 -> Nothing

{-# INLINE setLabelPosition #-}
setLabelPosition p c =
    case c of
        View Button_ {..} -> View Button_ { labelPosition = p, .. }
        _                 -> c

pattern Link c <- (getLink -> (True,c)) where
    Link c = setLink c

{-# INLINE getLink #-}
getLink c =
    case c of
        View Icon_     {..} -> (link,c)
        View List_     {..} -> (link,c)
        View ListIcon_ {..} -> (link,c)
        _                   -> (False,c)

{-# INLINE setLink #-}
setLink c =
    case c of
        View Icon_     {..} -> View Icon_     { link = True, .. }
        View List_     {..} -> View List_     { link = True, .. }
        View ListIcon_ {..} -> View ListIcon_ { link = True, .. }
        _                   -> c

pattern Loading c <- (getLoading -> (True,c)) where
    Loading c = setLoading c

{-# INLINE getLoading #-}
getLoading c =
    case c of
        View Button_   {..} -> (loading,c)
        View Icon_     {..} -> (loading,c)
        View Input_    {..} -> (loading,c)
        View ListIcon_ {..} -> (loading,c)
        _                   -> (False,c)

{-# INLINE setLoading #-}
setLoading c =
    case c of
        View Button_   {..} -> View Button_   { loading = True, .. }
        View Icon_     {..} -> View Icon_     { loading = True, .. }
        View Input_    {..} -> View Input_    { loading = True, .. }
        View ListIcon_ {..} -> View ListIcon_ { loading = True, .. }
        _                 -> c

pattern Localize t c <- (getLocalize -> Just (t,c)) where
    Localize t c = setLocalize t c

{-# INLINE getLocalize #-}
getLocalize c =
    case c of
        View ButtonOr_ {..} -> Just (localize,c)
        _                   -> Nothing

{-# INLINE setLocalize #-}
setLocalize l c =
    case c of
        View ButtonOr_ {..} -> View ButtonOr_ { localize = l, .. }
        _                   -> c

pattern Negative c <- (getNegative -> (True,c)) where
    Negative c = setNegative c

{-# INLINE getNegative #-}
getNegative c =
    case c of
        View Button_      {..} -> (negative,c)
        View ButtonGroup_ {..} -> (negative,c)
        _                      -> (False,c)

{-# INLINE setNegative #-}
setNegative c =
    case c of
        View Button_      {..} -> View Button_      { negative = True, .. }
        View ButtonGroup_ {..} -> View ButtonGroup_ { negative = True, .. }
        _                      -> c

pattern Ordered c <- (getOrdered -> (True,c)) where
    Ordered c = setOrdered c

{-# INLINE getOrdered #-}
getOrdered c =
    case c of
        View List_ {..} -> (ordered,c)
        _               -> (False,c)

{-# INLINE setOrdered #-}
setOrdered c =
    case c of
        View List_ {..} -> View List_ { ordered = True, .. }
        _               -> c

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

