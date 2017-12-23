module Semantic.Extensions where

import Pure.Data
import Pure.View (pattern View)

import Semantic.Elements.Button as Button
import Semantic.Elements.Container as Container
import Semantic.Elements.Label as Label
import Semantic.Elements.Icon as Icon
import Semantic.Elements.Image as Image

import Semantic.Utils

import Debug.Trace

infixl 1 !
(!) c cs = Children cs c

infixl 2 %
(%) c as = Attributes as c

infixl 1 !%
(!%) c cs as = Children (cs (Attributes as c))

infixl 1 %!
(%!) c as cs = Attributes (as (Children cs c))

pattern Children cs c <- (getChildren -> Just (cs,c)) where
    Children cs c = setChildren cs c

{-# INLINE getChildren #-}
getChildren c =
    case c of
        View Button_        {..} -> Just (children,c)
        View ButtonContent_ {..} -> Just (children,c)
        View Container_     {..} -> Just (children,c)
        View Label_         {..} -> Just (children,c)
        View LabelDetail_   {..} -> Just (children,c)
        View LabelGroup_    {..} -> Just (children,c)
        View IconGroup_     {..} -> Just (children,c)
        View Image_         {..} -> Just (children,c)
        View ImageGroup_    {..} -> Just (children,c)
        _                        -> Nothing

{-# INLINE setChildren #-}
setChildren cs c =
    case c of
        View Button_        {..} -> View Button_        { children = cs, .. }
        View ButtonContent_ {..} -> View ButtonContent_ { children = cs, .. }
        View Container_     {..} -> View Container_     { children = cs, .. }
        View Label_         {..} -> View Label_         { children = cs, .. }
        View LabelDetail_   {..} -> View LabelDetail_   { children = cs, .. }
        View LabelGroup_    {..} -> View LabelGroup_    { children = cs, .. }
        View IconGroup_     {..} -> View IconGroup_     { children = cs, .. }
        View Image_         {..} -> View Image_         { children = cs, .. }
        View ImageGroup_    {..} -> View ImageGroup_    { children = cs, .. }
        _                        -> c

pattern Classes cs c <- (getClasses -> Just (cs,c)) where
    Classes cs c = setClasses cs c

{-# INLINE getClasses #-}
getClasses c =
    case c of
        View Button_        {..} -> Just (classes,c)
        View ButtonContent_ {..} -> Just (classes,c)
        View Container_     {..} -> Just (classes,c)
        View Label_         {..} -> Just (classes,c)
        View LabelDetail_   {..} -> Just (classes,c)
        View LabelGroup_    {..} -> Just (classes,c)
        View Icon_          {..} -> Just (classes,c)
        View IconGroup_     {..} -> Just (classes,c)
        View Image_         {..} -> Just (classes,c)
        View ImageGroup_    {..} -> Just (classes,c)
        _                        -> Nothing

{-# INLINE setClasses #-}
setClasses cs c =
    case c of
        View Button_        {..} -> View Button_        { classes = cs, .. }
        View ButtonContent_ {..} -> View ButtonContent_ { classes = cs, .. }
        View Container_     {..} -> View Container_     { classes = cs, .. }
        View Label_         {..} -> View Label_         { classes = cs, .. }
        View LabelDetail_   {..} -> View LabelDetail_   { classes = cs, .. }
        View LabelGroup_    {..} -> View LabelGroup_    { classes = cs, .. }
        View Icon_          {..} -> View Icon_          { classes = cs, .. }
        View IconGroup_     {..} -> View IconGroup_     { classes = cs, .. }
        View Image_         {..} -> View Image_         { classes = cs, .. }
        View ImageGroup_    {..} -> View ImageGroup_    { classes = cs, .. }
        _                      -> c

pattern Attributes as c <- (getAttributes -> Just (as,c)) where
    Attributes as c = setAttributes as c

{-# INLINE getAttributes #-}
getAttributes c =
    case c of
        View Button_        {..} -> Just (attributes,c)
        View ButtonContent_ {..} -> Just (attributes,c)
        View Container_     {..} -> Just (attributes,c)
        View Label_         {..} -> Just (attributes,c)
        View LabelDetail_   {..} -> Just (attributes,c)
        View LabelGroup_    {..} -> Just (attributes,c)
        View Icon_          {..} -> Just (attributes,c)
        View IconGroup_     {..} -> Just (attributes,c)
        View Image_         {..} -> Just (attributes,c)
        View ImageGroup_    {..} -> Just (attributes,c)
        _                        -> Nothing

{-# INLINE setAttributes #-}
setAttributes cs c =
    case c of
        View Button_        {..} -> View Button_        { attributes = cs, .. }
        View ButtonContent_ {..} -> View ButtonContent_ { attributes = cs, .. }
        View Container_     {..} -> View Container_     { attributes = cs, .. }
        View Label_         {..} -> View Label_         { attributes = cs, .. }
        View LabelDetail_   {..} -> View LabelDetail_   { attributes = cs, .. }
        View LabelGroup_    {..} -> View LabelGroup_    { attributes = cs, .. }
        View Icon_          {..} -> View Icon_          { attributes = cs, .. }
        View IconGroup_     {..} -> View IconGroup_     { attributes = cs, .. }
        View Image_         {..} -> View Image_         { attributes = cs, .. }
        View ImageGroup_    {..} -> View ImageGroup_    { attributes = cs, .. }
        _                        -> c

pattern As as c <- (getAs -> Just (as,c)) where
    As as c = setAs as c

{-# INLINE getAs #-}
getAs c =
    case c of
        View Button_        {..} -> Just (as,c)
        View ButtonContent_ {..} -> Just (as,c)
        View Container_     {..} -> Just (as,c)
        View Label_         {..} -> Just (as,c)
        View LabelDetail_   {..} -> Just (as,c)
        View LabelGroup_    {..} -> Just (as,c)
        View Icon_          {..} -> Just (as,c)
        View IconGroup_     {..} -> Just (as,c)
        View Image_         {..} -> Just (as,c)
        View ImageGroup_    {..} -> Just (as,c)
        _                        -> Nothing

{-# INLINE setAs #-}
setAs a c =
    case c of
        View Button_        {..} -> View Button_        { as = a, .. }
        View ButtonContent_ {..} -> View ButtonContent_ { as = a, .. }
        View Container_     {..} -> View Container_     { as = a, .. }
        View Label_         {..} -> View Label_         { as = a, .. }
        View LabelDetail_   {..} -> View LabelDetail_   { as = a, .. }
        View LabelGroup_    {..} -> View LabelGroup_    { as = a, .. }
        View Icon_          {..} -> View Icon_          { as = a, .. }
        View IconGroup_     {..} -> View IconGroup_     { as = a, .. }
        View Image_         {..} -> View Image_         { as = a, .. }
        View ImageGroup_    {..} -> View ImageGroup_    { as = a, .. }
        _                        -> c

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

pattern Active c <- (getActive -> (True,c)) where
    Active c = setActive c

{-# INLINE getActive #-}
getActive c = 
    case c of
        View Button_ {..} -> (active,c)
        View Label_  {..} -> (active,c)
        _                 -> (False,c)

{-# INLINE setActive #-}
setActive c =
    case c of
        View Button_ {..} -> View Button_ { active = True, .. }
        View Label_  {..} -> View Label_  { active = True, .. }
        _                 -> c

pattern Animated a c <- (getAnimated -> Just (a,c)) where
    Animated a c = setAnimated a c

{-# INLINE getAnimated #-}
getAnimated c =
    case c of
        View Button_ {..} -> animated # Just (animated,c)
        _                 -> Nothing

{-# INLINE setAnimated #-}
setAnimated a c =
    case c of
        View Button_ {..} -> View Button_ { animated = a, .. }
        _                 -> c

pattern Attached a c <- (getAttached -> (Just a,c)) where
    Attached a c = setAttached a c

pattern ToLeft = "left"
pattern ToRight = "right"
pattern ToTop = "top"
pattern ToBottom = "bottom"

{-# INLINE getAttached #-}
getAttached c =
    case c of
        View Button_ {..} -> (attached,c)
        View Label_  {..} -> attached ? (Just attached,c) $ (Nothing,c)
        _                 -> (Nothing,c)

{-# INLINE setAttached #-}
setAttached a c =
    case c of
        View Button_ {..} -> View Button_ { attached = Just a, .. }
        View Label_  {..} -> View Label_  { attached = a, .. }
        _                 -> c

pattern Avatar c <- (getAvatar -> (True,c)) where
    Avatar c = setAvatar c

{-# INLINE getAvatar #-}
getAvatar c =
    case c of
        View Image_ {..} -> (avatar,c)
        _                -> (False,c)

{-# INLINE setAvatar #-} 
setAvatar c =
    case c of
        View Image_ {..} -> View Image_ { avatar = True, .. }
        _                -> c

-- assuming default of false for all basic-capable components
pattern Basic c <- (getBasic -> (True,c)) where
    Basic c = setBasic c

{-# INLINE getBasic #-}
getBasic c = 
    case c of
        View Button_ {..} -> (basic,c)
        View Label_  {..} -> (basic,c)
        _                 -> (False,c)

{-# INLINE setBasic #-}
setBasic c =
    case c of
        View Button_ {..} -> View Button_ { basic = True, .. }
        View Label_  {..} -> View Label_  { basic = True, .. }
        _                 -> c

pattern Bordered c <- (getBordered -> (True,c)) where
    Bordered c = setBordered c

{-# INLINE getBordered #-}
getBordered c =
    case c of
        View Icon_  {..} -> (bordered,c)
        View Image_ {..} -> (bordered,c)
        _                -> (False,c)

{-# INLINE setBordered #-}
setBordered c =
    case c of
        View Icon_  {..} -> View Icon_  { bordered = True, .. }
        View Image_ {..} -> View Image_ { bordered = True, .. }
        c                -> c

pattern Centered c <- (getCentered -> (True,c)) where
    Centered c = setCentered c

{-# INLINE getCentered #-}
getCentered c =
    case c of
        View Image_ {..} -> (centered,c)
        _                -> (False,c)

{-# INLINE setCentered #-}
setCentered c =
    case c of
        View Image_ {..} -> View Image_ { centered = True, .. }
        _                -> c

pattern Circular c <- (getCircular -> (True,c)) where
    Circular c = setCircular c

{-# INLINE getCircular #-}
getCircular c =
    case c of
        View Button_ {..} -> (circular,c)
        View Icon_   {..} -> (circular,c)
        View Image_  {..} -> (circular,c)
        View Label_  {..} -> (circular,c)
        View LabelGroup_ {..} -> (circular,c)
        _                 -> (False,c)

{-# INLINE setCircular #-}
setCircular c =
    case c of
        View Button_ {..} -> View Button_ { circular = True, .. }
        View Icon_   {..} -> View Icon_   { circular = True, .. }
        View Image_  {..} -> View Image_  { circular = True, .. }
        View Label_  {..} -> View Label_  { circular = True, .. }
        View LabelGroup_ {..} -> View LabelGroup_ { circular = True, .. }
        _                 -> c

pattern Color col c <- (getColor -> Just (col,c)) where
    Color col c = setColor col c

pattern Red = "red"
pattern Orange = "orange"
pattern Yellow = "yellow"
pattern Olive = "olive"
pattern Green = "green"
pattern Teal = "teal"
pattern Blue = "blue"
pattern Violet = "violet"
pattern Purple = "purple"
pattern Pink = "pink"
pattern Brown = "brown"
pattern Grey = "grey"
pattern Gray = "grey"
pattern Black = "black"

{-# INLINE getColor #-}
getColor c =
    case c of
        View Button_ {..} -> color # Just (color,c)
        View Icon_   {..} -> color # Just (color,c)
        View Label_  {..} -> color # Just (color,c)
        View LabelGroup_ {..} -> color # Just (color,c)
        _                 -> Nothing

{-# INLINE setColor #-}
setColor col c =
    case c of
        View Button_ {..} -> View Button_ { color = col, .. }
        View Icon_   {..} -> View Icon_   { color = col, .. }
        View Label_  {..} -> View Label_  { color = col, .. }
        View LabelGroup_ {..} -> View LabelGroup_ { color = col, .. }
        _                 -> c

pattern Corner cor c <- (getCorner -> (Just cor,c)) where
    Corner cor c = setCorner cor c

{-# INLINE getCorner #-}
getCorner c =
    case c of
        View Icon_  {..} -> corner ? (Just "",c) $ (Nothing,c)
        View Label_ {..} -> (corner,c)
        _                -> (Nothing,c)

{-# INLINE setCorner #-}
setCorner cor c =
    case c of
        View Icon_  {..} -> View Icon_  { corner = True, .. }
        View Label_ {..} -> View Label_ { corner = Just cor, .. }
        _                -> c

pattern Compact c <- (getCompact -> (True,c)) where
    Compact c = setCompact c

{-# INLINE getCompact #-}
getCompact c =
    case c of
        View Button_ {..} -> (compact,c)
        _                 -> (False,c)

{-# INLINE setCompact #-}
setCompact c =
    case c of
        View Button_ {..} -> View Button_ { compact = True, .. }
        _                 -> c

pattern Disabled c <- (getDisabled -> (True,c)) where
    Disabled c = setDisabled c

{-# INLINE getDisabled #-}
getDisabled c =
    case c of
        View Button_ {..} -> (disabled,c)
        View Icon_   {..} -> (disabled,c)
        View Image_  {..} -> (disabled,c)
        _                 -> (False,c)

{-# INLINE setDisabled #-}
setDisabled c =
    case c of
        View Button_ {..} -> View Button_ { disabled = True, .. }
        View Icon_   {..} -> View Icon_   { disabled = True, .. }
        View Image_  {..} -> View Image_  { disabled = True, .. }
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

pattern Fitted c <- (getFitted -> (True,c)) where
    Fitted c = setFitted c

{-# INLINE getFitted #-}
getFitted c =
    case c of
        View Icon_ {..} -> (fitted,c)
        _               -> (False,c)

{-# INLINE setFitted #-}
setFitted c =
    case c of
        View Icon_ {..} -> View Icon_ { fitted = True, .. }
        _               -> c

pattern Focus c <- (getFocus -> (True,c)) where
    Focus c = setFocus c

{-# INLINE getFocus #-}
getFocus c =
    case c of
        View Button_ {..} -> (focus,c)
        _                 -> (False,c)

{-# INLINE setFocus #-}
setFocus c =
    case c of
        View Button_ {..} -> View Button_ { focus = True, .. }
        _                 -> c

pattern Flipped f c <- (getFlipped -> Just (f,c)) where
    Flipped f c = setFlipped f c

{-# INLINE getFlipped #-}
getFlipped c =
    case c of
        View Icon_ {..} -> flipped # Just (flipped,c)
        _               -> Nothing

{-# INLINE setFlipped #-}
setFlipped f c =
    case c of
        View Icon_ {..} -> View Icon_ { flipped = f, .. }
        _               -> c

pattern Floated f c <- (getFloated -> Just (f,c)) where
    Floated f c = setFloated f c

{-# INLINE getFloated #-}
getFloated c =
    case c of
        View Button_ {..} -> floated # Just (floated,c)
        View Image_  {..} -> floated # Just (floated,c)
        _                 -> Nothing

{-# INLINE setFloated #-}
setFloated f c =
    case c of
        View Button_ {..} -> View Button_ { floated = f, .. }
        View Image_  {..} -> View Image_  { floated = f, .. }
        _                 -> c

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
        View Button_    {..} -> (fluid,c)
        View Container_ {..} -> (fluid,c)
        View Image_     {..} -> (fluid,c)
        _                    -> (False,c)

{-# INLINE setFluid #-}
setFluid c =
    case c of
        View Container_ {..} -> View Container_ { fluid = True, .. }
        View Button_    {..} -> View Button_    { fluid = True, .. }
        View Image_     {..} -> View Image_     { fluid = True, .. }
        _                -> c

pattern Hidden c <- (getHidden -> (True,c)) where
    Hidden c = setHidden c

{-# INLINE getHidden #-}
getHidden c =
    case c of
        View Image_         {..} -> (hidden,c)
        View ButtonContent_ {..} -> (hidden,c)
        _                        -> (False,c)

{-# INLINE setHidden #-}
setHidden c =
    case c of
        View Image_         {..} -> View Image_         { hidden = True, .. }
        View ButtonContent_ {..} -> View ButtonContent_ { hidden = True, .. }
        _                        -> c

pattern Horizontal c <- (getHorizontal -> (True,c)) where
    Horizontal c = setHorizontal c

{-# INLINE getHorizontal #-}
getHorizontal c = 
    case c of
        View Label_ {..} -> (horizontal,c)
        _                -> (False,c)

{-# INLINE setHorizontal #-}
setHorizontal c =
    case c of
        View Label_ {..} -> View Label_ { horizontal = True, .. }
        _                -> c

pattern Inline c <- (getInline -> (True,c)) where
    Inline c = setInline c

{-# INLINE getInline #-}
getInline c =
    case c of
        View Image_ {..} -> (inline,c)
        _                -> (False,c)

{-# INLINE setInline #-}
setInline c =
    case c of
        View Image_ {..} -> View Image_ { inline = True, .. }
        _                -> c

pattern Inverted c <- (getInverted -> (True,c)) where
    Inverted c = setInverted c

{-# INLINE getInverted #-}
getInverted c =
    case c of
        View Button_ {..} -> (inverted,c)
        View Icon_   {..} -> (inverted,c)
        _                 -> (False,c)

{-# INLINE setInverted #-}
setInverted c =
    case c of
        View Button_ {..} -> View Button_ { inverted = True, .. }
        View Icon_   {..} -> View Icon_   { inverted = True, .. }
        _                 -> c

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
        View Icon_ {..} -> (link,c)
        _               -> (False,c)

{-# INLINE setLink #-}
setLink c =
    case c of
        View Icon_ {..} -> View Icon_ { link = True, .. }
        _               -> c

pattern Loading c <- (getLoading -> (True,c)) where
    Loading c = setLoading c

{-# INLINE getLoading #-}
getLoading c =
    case c of
        View Button_ {..} -> (loading,c)
        View Icon_   {..} -> (loading,c)
        _                 -> (False,c)

{-# INLINE setLoading #-}
setLoading c =
    case c of
        View Button_ {..} -> View Button_ { loading = True, .. }
        View Icon_   {..} -> View Icon_   { loading = True, .. }
        _                 -> c

pattern Name n c <- (getName -> Just (n,c)) where
    Name n c = setName n c

{-# INLINE getName #-}
getName c =
    case c of
        View Icon_ {..} -> name # Just (name,c)
        _               -> Nothing

{-# INLINE setName #-}
setName n c =
    case c of
        View Icon_ {..} -> View Icon_ { name = n, .. }
        _               -> c

pattern Negative c <- (getNegative -> (True,c)) where
    Negative c = setNegative c

{-# INLINE getNegative #-}
getNegative c =
    case c of
        View Button_ {..} -> (negative,c)
        _                 -> (False,c)

{-# INLINE setNegative #-}
setNegative c =
    case c of
        View Button_ {..} -> View Button_ { negative = True, .. }
        _                 -> c

pattern HandleClick h c <- (getHandleClick -> Just (h,c)) where
    HandleClick h c = setHandleClick h c

{-# INLINE getHandleClick #-}
getHandleClick c =
    case c of
        View Button_ {..} -> handleClick # Just (handleClick,c)
        View Label_  {..} -> handleClick # Just (handleClick,c)
        _                 -> Nothing

{-# INLINE setHandleClick #-}
setHandleClick h c =
    case c of
        View Button_ {..} -> View Button_ { handleClick = h, .. }
        View Label_  {..} -> View Label_  { handleClick = h, .. }
        _                 -> c

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

pattern Positive c <- (getPositive -> (True,c)) where
    Positive c = setPositive c

{-# INLINE getPositive #-}
getPositive c =
    case c of
        View Button_ {..} -> (positive,c)
        _                 -> (False,c)

{-# INLINE setPositive #-}
setPositive c =
    case c of
        View Button_ {..} -> View Button_ { positive = True, .. }
        _                 -> c

pattern Primary c <- (getPrimary -> (True,c)) where
    Primary c = setPrimary c

{-# INLINE getPrimary #-}
getPrimary c =
    case c of
        View Button_ {..} -> (primary,c)
        _                 -> (False,c)

{-# INLINE setPrimary #-}
setPrimary c =
    case c of
        View Button_ {..} -> View Button_ { primary = True, .. }
        _                 -> c

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
        View Icon_ {..} -> rotated # Just (rotated,c)
        _               -> Nothing

{-# INLINE setRotated #-}
setRotated r c =
    case c of
        View Icon_ {..} -> View Icon_ { rotated = r, .. }
        _               -> c

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
        View Button_ {..} -> (secondary,c)
        _                 -> (False,c)

{-# INLINE setSecondary #-}
setSecondary c =
    case c of
        View Button_ {..} -> View Button_ { secondary = True, .. }
        _                 -> c

pattern Mini c = Size "mini" c
pattern Tiny c = Size "tiny" c
pattern Small c = Size "small" c
pattern Medium c = Size "medium" c
pattern Large c = Size "large" c
pattern Big c = Size "big" c
pattern Huge c = Size "huge" c
pattern Massive c = Size "massive" c

pattern Size s c <- (getSize -> Just (s,c)) where
    Size s c = setSize s c

{-# INLINE getSize #-}
getSize c =
    case c of
        View Button_ {..} -> size # Just (size,c)
        View Icon_   {..} -> size # Just (size,c)
        View IconGroup_  {..} -> size # Just (size,c)
        View Image_  {..} -> size # Just (size,c)
        View ImageGroup_ {..} -> size # Just (size,c)
        View Label_  {..} -> size # Just (size,c)
        View LabelGroup_ {..} -> size # Just (size,c)
        _                 -> Nothing

{-# INLINE setSize #-}
setSize s c =
    case c of
        View Button_ {..} -> View Button_ { size = s, .. }
        View Icon_   {..} -> View Icon_   { size = s, .. }
        View IconGroup_ {..} -> View IconGroup_ { size = s, .. }
        View Image_  {..} -> View Image_  { size = s, .. }
        View ImageGroup_  {..} -> View ImageGroup_  { size = s, .. }
        View Label_  {..} -> View Label_  { size = s, .. }
        View LabelGroup_ {..} -> View LabelGroup_ { size = s, .. }
        _                 -> c

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

pattern TabIndex n c <- (getTabIndex -> Just (n,c)) where
  TabIndex n c = setTabIndex n c

{-# INLINE getTabIndex #-}
getTabIndex c =
    case c of
        View Button_ {..} -> Just (tabIndex,c)
        _                 -> Nothing

{-# INLINE setTabIndex #-}
setTabIndex n c =
    case c of
        View Button_ {..} -> View Button_ { tabIndex = n, .. }
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

pattern Toggle c <- (getToggle -> (True,c)) where
    Toggle c = setToggle c

{-# INLINE getToggle #-}
getToggle c =
    case c of
        View Button_ {..} -> (toggle,c)
        _                 -> (False,c)

{-# INLINE setToggle #-}
setToggle c =
    case c of
        View Button_ {..} -> View Button_ { toggle = True, .. }
        _                 -> c

pattern UI c <- (getUI -> (True,c)) where
    UI c = setUI c

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

pattern VerticalAlign va c <- (getVerticalAlign -> Just (va,c)) where
    VerticalAlign va c = setVerticalAlign va c

pattern Middle = "middle"
-- pattern Bottom = "bottom"
-- pattern Top = "top"

{-# INLINE getVerticalAlign #-}
getVerticalAlign c =
    case c of
        View Image_ {..} -> Just (verticalAlign,c)
        _                -> Nothing

{-# INLINE setVerticalAlign #-}
setVerticalAlign va c =
    case c of
        View Image_ {..} -> View Image_ { verticalAlign = va, .. }
        _                -> c

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

pattern TextAlignLeft c = TextAlign AlignedLeft c
pattern TextAlignCenter c = TextAlign AlignedCenter c
pattern TextAlignRight c = TextAlign AlignedRight c
pattern TextAlignJustified c = TextAlign AlignedJustified c

pattern Animate = "animate"
-- pattern Fade = "fade"
pattern Vertical = "vertical"

----------------------------------

pattern NamedIcon i name = Name name (Icon i)

pattern TextContainer c <- (getTextContainer -> (True,c)) where
    TextContainer c = setTextContainer c

{-# INLINE getTextContainer #-}
getTextContainer c =
    case c of
        View Container_ {..} -> (text,c)
        _                    -> (False,c)

{-# INLINE setTextContainer #-}
setTextContainer c =
    case c of
        View Container_ {..} -> View Container_ { text = True, .. }
        _                    -> c


