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

infixl 2 %
(%) c as = Attributes as c

-- infixl 1 !%
-- (!%) c cs as = Children (cs (Attributes as c))

-- infixl 1 %!
-- (%!) c as cs = Attributes (as (Children cs c))

pattern Classes cs c <- (getClasses -> Just (cs,c)) where
    Classes cs c = setClasses cs c

{-# INLINE getClasses #-}
getClasses c =
    case c of
        View Button_          {..} -> Just (classes,c)
        View ButtonContent_   {..} -> Just (classes,c)
        View ButtonOr_        {..} -> Just (classes,c)
        View ButtonGroup_     {..} -> Just (classes,c)
        View Container_       {..} -> Just (classes,c)
        View Divider_         {..} -> Just (classes,c)
        View Header_          {..} -> Just (classes,c)
        View HeaderContent_   {..} -> Just (classes,c)
        View HeaderSubheader_ {..} -> Just (classes,c)
        View Icon_            {..} -> Just (classes,c)
        View IconGroup_       {..} -> Just (classes,c)
        View Image_           {..} -> Just (classes,c)
        View ImageGroup_      {..} -> Just (classes,c)
        View Input_           {..} -> Just (classes,c)
        View Label_           {..} -> Just (classes,c)
        View LabelDetail_     {..} -> Just (classes,c)
        View LabelGroup_      {..} -> Just (classes,c)
        View List_            {..} -> Just (classes,c)
        View ListContent_     {..} -> Just (classes,c)
        View ListDescription_ {..} -> Just (classes,c)
        View ListHeader_      {..} -> Just (classes,c)
        View ListIcon_        {..} -> Just (classes,c)
        View ListItem_        {..} -> Just (classes,c)
        View ListList_        {..} -> Just (classes,c)
        View Loader_          {..} -> Just (classes,c)
        View Rail_            {..} -> Just (classes,c)
        _                          -> Nothing

{-# INLINE setClasses #-}
setClasses cs c =
    case c of
        View Button_          {..} -> View Button_          { classes = cs, .. }
        View ButtonGroup_     {..} -> View ButtonGroup_     { classes = cs, .. }
        View ButtonOr_        {..} -> View ButtonOr_        { classes = cs, .. }
        View Container_       {..} -> View Container_       { classes = cs, .. }
        View Divider_         {..} -> View Divider_         { classes = cs, .. }
        View Header_          {..} -> View Header_          { classes = cs, .. }
        View HeaderContent_   {..} -> View HeaderContent_   { classes = cs, .. }
        View HeaderSubheader_ {..} -> View HeaderSubheader_ { classes = cs, .. }
        View Icon_            {..} -> View Icon_            { classes = cs, .. }
        View IconGroup_       {..} -> View IconGroup_       { classes = cs, .. }
        View Image_           {..} -> View Image_           { classes = cs, .. }
        View ImageGroup_      {..} -> View ImageGroup_      { classes = cs, .. }
        View Input_           {..} -> View Input_           { classes = cs, .. }
        View Label_           {..} -> View Label_           { classes = cs, .. }
        View LabelDetail_     {..} -> View LabelDetail_     { classes = cs, .. }
        View LabelGroup_      {..} -> View LabelGroup_      { classes = cs, .. }
        View List_            {..} -> View List_            { classes = cs, .. }
        View ListContent_     {..} -> View ListContent_     { classes = cs, .. }
        View ListDescription_ {..} -> View ListDescription_ { classes = cs, .. }
        View ListHeader_      {..} -> View ListHeader_      { classes = cs, .. }
        View ListIcon_        {..} -> View ListIcon_        { classes = cs, .. }
        View ListItem_        {..} -> View ListItem_        { classes = cs, .. }
        View ListList_        {..} -> View ListList_        { classes = cs, .. }
        View Loader_          {..} -> View Loader_          { classes = cs, .. }
        View Rail_            {..} -> View Rail_            { classes = cs, .. }
        _                          -> c

pattern Attributes as c <- (getAttributes -> Just (as,c)) where
    Attributes as c = setAttributes as c

{-# INLINE getAttributes #-}
getAttributes c =
    case c of
        View Button_          {..} -> Just (attributes,c)
        View ButtonContent_   {..} -> Just (attributes,c)
        View ButtonGroup_     {..} -> Just (attributes,c)
        View ButtonOr_        {..} -> Just (attributes,c)
        View Container_       {..} -> Just (attributes,c)
        View Divider_         {..} -> Just (attributes,c)
        View Header_          {..} -> Just (attributes,c)
        View HeaderContent_   {..} -> Just (attributes,c)
        View HeaderSubheader_ {..} -> Just (attributes,c)
        View Icon_            {..} -> Just (attributes,c)
        View IconGroup_       {..} -> Just (attributes,c)
        View Image_           {..} -> Just (attributes,c)
        View ImageGroup_      {..} -> Just (attributes,c)
        View Input_           {..} -> Just (attributes,c)
        View Label_           {..} -> Just (attributes,c)
        View LabelDetail_     {..} -> Just (attributes,c)
        View LabelGroup_      {..} -> Just (attributes,c)
        View List_            {..} -> Just (attributes,c)
        View ListContent_     {..} -> Just (attributes,c)
        View ListDescription_ {..} -> Just (attributes,c)
        View ListHeader_      {..} -> Just (attributes,c)
        View ListIcon_        {..} -> Just (attributes,c)
        View ListItem_        {..} -> Just (attributes,c)
        View ListList_        {..} -> Just (attributes,c)
        View Loader_          {..} -> Just (attributes,c)
        View Rail_            {..} -> Just (attributes,c)
        _                          -> Nothing

{-# INLINE setAttributes #-}
setAttributes cs c =
    case c of
        View Button_          {..} -> View Button_          { attributes = cs, .. }
        View ButtonContent_   {..} -> View ButtonContent_   { attributes = cs, .. }
        View ButtonGroup_     {..} -> View ButtonGroup_     { attributes = cs, .. }
        View ButtonOr_        {..} -> View ButtonOr_        { attributes = cs, .. }
        View Container_       {..} -> View Container_       { attributes = cs, .. }
        View Divider_         {..} -> View Divider_         { attributes = cs, .. }
        View Header_          {..} -> View Header_          { attributes = cs, .. }
        View HeaderContent_   {..} -> View HeaderContent_   { attributes = cs, .. }
        View HeaderSubheader_ {..} -> View HeaderSubheader_ { attributes = cs, .. }
        View Icon_            {..} -> View Icon_            { attributes = cs, .. }
        View IconGroup_       {..} -> View IconGroup_       { attributes = cs, .. }
        View Image_           {..} -> View Image_           { attributes = cs, .. }
        View ImageGroup_      {..} -> View ImageGroup_      { attributes = cs, .. }
        View Input_           {..} -> View Input_           { attributes = cs, .. }
        View Label_           {..} -> View Label_           { attributes = cs, .. }
        View LabelDetail_     {..} -> View LabelDetail_     { attributes = cs, .. }
        View LabelGroup_      {..} -> View LabelGroup_      { attributes = cs, .. }
        View List_            {..} -> View List_            { attributes = cs, .. }
        View ListContent_     {..} -> View ListContent_     { attributes = cs, .. }
        View ListDescription_ {..} -> View ListDescription_ { attributes = cs, .. }
        View ListHeader_      {..} -> View ListHeader_      { attributes = cs, .. }
        View ListIcon_        {..} -> View ListIcon_        { attributes = cs, .. }
        View ListItem_        {..} -> View ListItem_        { attributes = cs, .. }
        View ListList_        {..} -> View ListList_        { attributes = cs, .. }
        View Loader_          {..} -> View Loader_          { attributes = cs, .. }
        View Rail_            {..} -> View Rail_            { attributes = cs, .. }
        _                          -> c

pattern As :: VC ms => ([Feature ms] -> [View ms] -> View ms) -> View ms -> View ms
pattern As as c <- (getAs -> Just (as,c)) where
    As as c = setAs as c

{-# INLINE getAs #-}
getAs c =
    case c of
        View Button_          {..} -> Just (as,c)
        View ButtonContent_   {..} -> Just (as,c)
        View ButtonGroup_     {..} -> Just (as,c)
        View ButtonOr_        {..} -> Just (as,c)
        View Container_       {..} -> Just (as,c)
        View Divider_         {..} -> Just (as,c)
        View Header_          {..} -> Just (as,c)
        View HeaderContent_   {..} -> Just (as,c)
        View HeaderSubheader_ {..} -> Just (as,c)
        View Icon_            {..} -> Just (as,c)
        View IconGroup_       {..} -> Just (as,c)
        View Image_           {..} -> Just (as,c)
        View ImageGroup_      {..} -> Just (as,c)
        View Input_           {..} -> Just (as,c)
        View Label_           {..} -> Just (as,c)
        View LabelDetail_     {..} -> Just (as,c)
        View LabelGroup_      {..} -> Just (as,c)
        View List_            {..} -> Just (as,c)
        View ListContent_     {..} -> Just (as,c)
        View ListDescription_ {..} -> Just (as,c)
        View ListHeader_      {..} -> Just (as,c)
        View ListIcon_        {..} -> Just (as,c)
        View ListItem_        {..} -> Just (as,c)
        View ListList_        {..} -> Just (as,c)
        View Loader_          {..} -> Just (as,c)
        View Rail_            {..} -> Just (as,c)
        _                          -> Nothing

{-# INLINE setAs #-}
setAs a c =
    case c of
        View Button_          {..} -> View Button_          { as = a, .. }
        View ButtonContent_   {..} -> View ButtonContent_   { as = a, .. }
        View ButtonGroup_     {..} -> View ButtonGroup_     { as = a, .. }
        View ButtonOr_        {..} -> View ButtonOr_        { as = a, .. }
        View Container_       {..} -> View Container_       { as = a, .. }
        View Divider_         {..} -> View Divider_         { as = a, .. }
        View Header_          {..} -> View Header_          { as = a, .. }
        View HeaderContent_   {..} -> View HeaderContent_   { as = a, .. }
        View HeaderSubheader_ {..} -> View HeaderSubheader_ { as = a, .. }
        View Icon_            {..} -> View Icon_            { as = a, .. }
        View IconGroup_       {..} -> View IconGroup_       { as = a, .. }
        View Image_           {..} -> View Image_           { as = a, .. }
        View ImageGroup_      {..} -> View ImageGroup_      { as = a, .. }
        View Input_           {..} -> View Input_           { as = a, .. }
        View Label_           {..} -> View Label_           { as = a, .. }
        View LabelDetail_     {..} -> View LabelDetail_     { as = a, .. }
        View LabelGroup_      {..} -> View LabelGroup_      { as = a, .. }
        View List_            {..} -> View List_            { as = a, .. }
        View ListContent_     {..} -> View ListContent_     { as = a, .. }
        View ListDescription_ {..} -> View ListDescription_ { as = a, .. }
        View ListHeader_      {..} -> View ListHeader_      { as = a, .. }
        View ListIcon_        {..} -> View ListIcon_        { as = a, .. }
        View ListItem_        {..} -> View ListItem_        { as = a, .. }
        View ListList_        {..} -> View ListList_        { as = a, .. }
        View Loader_          {..} -> View Loader_          { as = a, .. }
        View Rail_            {..} -> View Rail_            { as = a, .. }
        _                          -> c

pattern Active c <- (getActive -> (True,c)) where
    Active c = setActive c

{-# INLINE getActive #-}
getActive c = 
    case c of
        View Button_   {..} -> (active,c)
        View Label_    {..} -> (active,c)
        View ListItem_ {..} -> (active,c)
        View Loader_   {..} -> (active,c)
        _                   -> (False,c)

{-# INLINE setActive #-}
setActive c =
    case c of
        View Button_   {..} -> View Button_   { active = True, .. }
        View Label_    {..} -> View Label_    { active = True, .. }
        View ListItem_ {..} -> View ListItem_ { active = True, .. }
        View Loader_   {..} -> View Loader_   { active = True, .. }
        _                   -> c

pattern Animated a c <- (getAnimated -> Just (a,c)) where
    Animated a c = setAnimated a c

{-# INLINE getAnimated #-}
getAnimated c =
    case c of
        View Button_ {..} -> animated # Just (animated,c)
        View List_   {..} -> animated # Just (Just "",c)
        _                 -> Nothing

{-# INLINE setAnimated #-}
setAnimated a c =
    case c of
        View Button_ {..} -> View Button_ { animated = a, .. }
        View List_   {..} -> View List_   { animated = True, .. }
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
        View Button_      {..} -> (attached,c)
        View ButtonGroup_ {..} -> (attached,c)
        View Header_      {..} -> (attached,c)
        View Label_       {..} -> attached ? (Just attached,c) $ (Nothing,c)
        View Rail_        {..} -> attached ? (Just "",c) $ (Nothing,c)
        _                      -> (Nothing,c)

{-# INLINE setAttached #-}
setAttached a c =
    case c of
        View Button_      {..} -> View Button_      { attached = Just a, .. }
        View ButtonGroup_ {..} -> View ButtonGroup_ { attached = Just a, .. }
        View Header_      {..} -> View Header_      { attached = Just a, .. }
        View Label_       {..} -> View Label_       { attached = a, .. }
        View Rail_        {..} -> View Rail_        { attached = True, ..}
        _                      -> c

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
        View Button_      {..} -> (basic,c)
        View ButtonGroup_ {..} -> (basic,c)
        View Label_       {..} -> (basic,c)
        _                      -> (False,c)

{-# INLINE setBasic #-}
setBasic c =
    case c of
        View Button_      {..} -> View Button_      { basic = True, .. }
        View ButtonGroup_ {..} -> View ButtonGroup_ { basic = True, .. }
        View Label_       {..} -> View Label_       { basic = True, .. }
        _                      -> c

pattern Block c <- (getBlock -> (True,c)) where
    Block c = setBlock c

{-# INLINE getBlock #-}
getBlock c =
    case c of
        View Header_ {..} -> (block,c)
        _                 -> (False,c)

{-# INLINE setBlock #-}
setBlock c =
    case c of
        View Header_ {..} -> View Header_ { block = True, .. }
        _                 -> c

pattern Bordered c <- (getBordered -> (True,c)) where
    Bordered c = setBordered c

{-# INLINE getBordered #-}
getBordered c =
    case c of
        View Icon_     {..} -> (bordered,c)
        View Image_    {..} -> (bordered,c)
        View ListIcon_ {..} -> (bordered,c)
        _                    -> (False,c)

{-# INLINE setBordered #-}
setBordered c =
    case c of
        View Icon_     {..} -> View Icon_      { bordered = True, .. }
        View Image_    {..} -> View Image_     { bordered = True, .. }
        View ListIcon_ {..} -> View ListIcon_  { bordered = True, .. }
        c                   -> c

pattern Bulleted c <- (getBulleted -> (True,c)) where
    Bulleted c = setBulleted c

{-# INLINE getBulleted #-}
getBulleted c =
    case c of
        View List_ {..} -> (bulleted,c)
        _               -> (False,c)

{-# INLINE setBulleted #-}
setBulleted c =
    case c of
        View List_ {..} -> View List_ { bulleted = True, .. }
        _               -> c

pattern Celled c <- (getCelled -> (True,c)) where
    Celled c = setCelled c

{-# INLINE getCelled #-}
getCelled c =
    case c of
        View List_ {..} -> (celled,c)
        _               -> (False,c)

{-# INLINE setCelled #-}
setCelled c =
    case c of
        View List_ {..} -> View List_ { celled = True, .. }
        _               -> c

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

pattern Change f c <- (getChange -> Just (f,c)) where
    Change f c = setChange f c

{-# INLINE getChange #-}
getChange c =
    case c of
        View Input_ {..} -> Just (onChange,c)
        _                -> Nothing

{-# INLINE setChange #-}
setChange f c =
    case c of
        View Input_ {..} -> View Input_ { onChange = f, .. }
        _                -> c

pattern Circular c <- (getCircular -> (True,c)) where
    Circular c = setCircular c

{-# INLINE getCircular #-}
getCircular c =
    case c of
        View Button_     {..} -> (circular,c)
        View Icon_       {..} -> (circular,c)
        View Image_      {..} -> (circular,c)
        View Label_      {..} -> (circular,c)
        View LabelGroup_ {..} -> (circular,c)
        View ListIcon_   {..} -> (circular,c)
        _                 -> (False,c)

{-# INLINE setCircular #-}
setCircular c =
    case c of
        View Button_     {..} -> View Button_     { circular = True, .. }
        View Icon_       {..} -> View Icon_       { circular = True, .. }
        View Image_      {..} -> View Image_      { circular = True, .. }
        View Label_      {..} -> View Label_      { circular = True, .. }
        View LabelGroup_ {..} -> View LabelGroup_ { circular = True, .. }
        View ListIcon_   {..} -> View ListIcon_   { circular = True, .. }
        _                     -> c

pattern Clearing c <- (getClearing -> (True,c)) where
    Clearing c = setClearing c

{-# INLINE getClearing #-}
getClearing c =
    case c of
        View Divider_ {..} -> (clearing,c)
        _                  -> (False,c)

{-# INLINE setClearing #-}
setClearing c =
    case c of
        View Divider_ {..} -> View Divider_ { clearing = True, .. }
        _                  -> c

pattern Click h c <- (getClick -> Just (h,c)) where
    Click h c = setClick h c

{-# INLINE getClick #-}
getClick c =
    case c of
        View Button_   {..} -> click # Just (click,c)
        View Label_    {..} -> click # Just (click,c)
        View ListItem_ {..} -> click # Just (click,c)
        _                   -> Nothing

{-# INLINE setClick #-}
setClick h c =
    case c of
        View Button_   {..} -> View Button_   { click = h, .. }
        View Label_    {..} -> View Label_    { click = h, .. }
        View ListItem_ {..} -> View ListItem_ { click = h, .. }
        _                   -> c

pattern Close t c <- (getClose -> (Just t,c)) where
    Close t c = setClose t c

{-# INLINE getClose #-}
getClose c =
    case c of
        View Rail_ {..} -> (close,c)
        _               -> (Nothing,c)

{-# INLINE setClose #-}
setClose t c =
    case c of
        View Rail_ {..} -> View Rail_ { close = Just t, .. }
        _               -> c

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
        View Button_      {..} -> color # Just (color,c)
        View ButtonGroup_ {..} -> color # Just (color,c)
        View Header_      {..} -> color # Just (color,c)
        View Icon_        {..} -> color # Just (color,c)
        View Label_       {..} -> color # Just (color,c)
        View LabelGroup_  {..} -> color # Just (color,c)
        View ListIcon_    {..} -> color # Just (color,c)
        _                      -> Nothing

{-# INLINE setColor #-}
setColor col c =
    case c of
        View Button_      {..} -> View Button_      { color = col, .. }
        View ButtonGroup_ {..} -> View ButtonGroup_ { color = col, .. }
        View Header_      {..} -> View Header_      { color = col, .. }
        View Icon_        {..} -> View Icon_        { color = col, .. }
        View Label_       {..} -> View Label_       { color = col, .. }
        View LabelGroup_  {..} -> View LabelGroup_  { color = col, .. }
        View ListIcon_    {..} -> View ListIcon_    { color = col, .. }
        _                      -> c

pattern Corner cor c <- (getCorner -> (Just cor,c)) where
    Corner cor c = setCorner cor c

{-# INLINE getCorner #-}
getCorner c =
    case c of
        View Icon_     {..} -> corner ? (Just "",c) $ (Nothing,c)
        View Label_    {..} -> (corner,c)
        View ListIcon_ {..} -> corner ? (Just "",c) $ (Nothing,c)
        _                -> (Nothing,c)

{-# INLINE setCorner #-}
setCorner cor c =
    case c of
        View Icon_     {..} -> View Icon_     { corner = True, .. }
        View Label_    {..} -> View Label_    { corner = Just cor, .. }
        View ListIcon_ {..} -> View ListIcon_ { corner = True, .. }
        _                   -> c

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

pattern ItemClick f c <- (getItemClick -> Just (f,c)) where
    ItemClick f c = setItemClick f c

{-# INLINE getItemClick #-}
getItemClick c =
    case c of
        View List_ {..} -> Just (itemClick,c)
        _               -> Nothing

{-# INLINE setItemClick #-}
setItemClick f c =
    case c of
        View List_ {..} -> View List_ { itemClick = f, .. }
        _               -> c

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


