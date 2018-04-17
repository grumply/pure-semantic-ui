module Semantic.Segment
  ( module Properties
  , module Tools
  , Segment(..), pattern Segment
  , Group(..), pattern Group
  ) where

import GHC.Generics as G
import Pure.View hiding (color,disabled,textAlign,vertical,horizontal)

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>), (!) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attached, Attached(..)
  , pattern Attributes, Attributes(..)
  , pattern Basic, Basic(..)
  , pattern Children, Children(..)
  , pattern Circular, Circular(..)
  , pattern Classes, Classes(..)
  , pattern Clearing, Clearing(..)
  , pattern Color, Color(..)
  , pattern Compact, Compact(..)
  , pattern Disabled, Disabled(..)
  , pattern Floated, Floated(..)
  , pattern Inverted, Inverted(..)
  , pattern Loading, Loading(..)
  , pattern Padded, Padded(..)
  , pattern Piled, Piled(..)
  , pattern Raised, Raised(..)
  , pattern Secondary, Secondary(..)
  , pattern Size, Size(..)
  , pattern Stacked, Stacked(..)
  , pattern Tertiary, Tertiary(..)
  , pattern TextAlign, TextAlign(..)
  , pattern Vertical, Vertical(..)
  , pattern Horizontal, Horizontal(..)
  )

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

data Segment ms = Segment_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , attached :: Txt
    , basic :: Bool
    , circular :: Bool
    , clearing :: Bool
    , color :: Txt
    , compact :: Bool
    , disabled :: Bool
    , floated :: Txt
    , inverted :: Bool
    , loading :: Bool
    , padded :: Maybe Txt
    , piled :: Bool
    , raised :: Bool
    , secondary :: Bool
    , size :: Txt
    , stacked :: Bool
    , tertiary :: Bool
    , textAlign :: Txt
    , vertical :: Bool
    } deriving (Generic)

instance Default (Segment ms) where
    def = (G.to gdef) { as = Div }

pattern Segment :: Segment ms -> View ms
pattern Segment s = View s

instance Pure Segment ms where
    render Segment_ {..} =
        let
            cs =
                ( "ui"
                : color
                : size
                : basic # "basic"
                : circular # "circular"
                : clearing # "clearing"
                : compact # "compact"
                : disabled # "disabled"
                : inverted # "inverted"
                : loading # "loading"
                : piled # "piled"
                : raised # "raised"
                : secondary # "secondary"
                : stacked # "stacked"
                : tertiary # "tertiary"
                : vertical # "vertical"
                : attached # (attached <>> "attached")
                : may (<>> "padded") padded
                : textAlign
                : floated # ("floated" <<>> floated)
                : "segment"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (Segment ms) where
    type Prop As (Segment ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a s = s { as = a }

instance HasProp Attached (Segment ms) where
    type Prop Attached (Segment ms) = Txt
    getProp _ = attached
    setProp _ a s = s { attached = a }

instance HasProp Attributes (Segment ms) where
    type Prop Attributes (Segment ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as s = s { attributes = as }

instance HasProp Basic (Segment ms) where
    type Prop Basic (Segment ms) = Bool
    getProp _ = basic
    setProp _ b s = s { basic = b }

instance HasProp Children (Segment ms) where
    type Prop Children (Segment ms) = [View ms]
    getProp _ = children
    setProp _ cs s = s { children = cs }

instance HasProp Circular (Segment ms) where
    type Prop Circular (Segment ms) = Bool
    getProp _ = circular
    setProp _ c s = s { circular = c }

instance HasProp Classes (Segment ms) where
    type Prop Classes (Segment ms) = [Txt]
    getProp _ = classes
    setProp _ cs s = s { classes = cs }

instance HasProp Clearing (Segment ms) where
    type Prop Clearing (Segment ms) = Bool
    getProp _ = clearing
    setProp _ c s = s { clearing = c }

instance HasProp Color (Segment ms) where
    type Prop Color (Segment ms) = Txt
    getProp _ = color
    setProp _ c s = s { color = c }

instance HasProp Compact (Segment ms) where
    type Prop Compact (Segment ms) = Bool
    getProp _ = compact
    setProp _ c s = s { compact = c }

instance HasProp Disabled (Segment ms) where
    type Prop Disabled (Segment ms) = Bool
    getProp _ = disabled
    setProp _ d s = s { disabled = d }

instance HasProp Floated (Segment ms) where
    type Prop Floated (Segment ms) = Txt
    getProp _ = floated
    setProp _ f s = s { floated = f }

instance HasProp Inverted (Segment ms) where
    type Prop Inverted (Segment ms) = Bool
    getProp _ = inverted
    setProp _ i s = s { inverted = i }

instance HasProp Loading (Segment ms) where
    type Prop Loading (Segment ms) = Bool
    getProp _ = loading
    setProp _ l s = s { loading = l }

instance HasProp Padded (Segment ms) where
    type Prop Padded (Segment ms) = Maybe Txt
    getProp _ = padded
    setProp _ p s = s { padded = p }

instance HasProp Piled (Segment ms) where
    type Prop Piled (Segment ms) = Bool
    getProp _ = piled
    setProp _ p s = s { piled = p }

instance HasProp Raised (Segment ms) where
    type Prop Raised (Segment ms) = Bool
    getProp _ = raised
    setProp _ r s = s { raised = r }

instance HasProp Secondary (Segment ms) where
    type Prop Secondary (Segment ms) = Bool
    getProp _ = secondary
    setProp _ sec s = s { secondary = sec }

instance HasProp Size (Segment ms) where
    type Prop Size (Segment ms) = Txt
    getProp _ = size
    setProp _ sz s = s { size = sz }

instance HasProp Stacked (Segment ms) where
    type Prop Stacked (Segment ms) = Bool
    getProp _ = stacked
    setProp _ stkd s = s { stacked = stkd }

instance HasProp Tertiary (Segment ms) where
    type Prop Tertiary (Segment ms) = Bool
    getProp _ = tertiary
    setProp _ t s = s { tertiary = t }

instance HasProp TextAlign (Segment ms) where
    type Prop TextAlign (Segment ms) = Txt
    getProp _ = textAlign
    setProp _ ta s = s { textAlign = ta }

instance HasProp Vertical (Segment ms) where
    type Prop Vertical (Segment ms) = Bool
    getProp _ = vertical
    setProp _ v s = s { vertical = v }

data Group ms = Group_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , compact :: Bool
    , horizontal :: Bool
    , piled :: Bool
    , raised :: Bool
    , size :: Txt
    , stacked :: Bool
    } deriving (Generic)

instance Default (Group ms) where
    def = (G.to gdef) { as = Div }

pattern Group :: Group ms -> View ms
pattern Group sg = View sg

instance Pure Group ms where
    render Group_ {..} =
        let
            cs =
                ( "ui"
                : size
                : compact # "compact"
                : horizontal # "horizontal"
                : piled # "piled"
                : raised # "raised"
                : stacked # "stacked"
                : "segments"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (Group ms) where
    type Prop As (Group ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a sg = sg { as = a }

instance HasProp Attributes (Group ms) where
    type Prop Attributes (Group ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as sg = sg { attributes = as }

instance HasProp Children (Group ms) where
    type Prop Children (Group ms) = [View ms]
    getProp _ = children
    setProp _ cs sg = sg { children = cs }

instance HasProp Classes (Group ms) where
    type Prop Classes (Group ms) = [Txt]
    getProp _ = classes
    setProp _ cs sg = sg { classes = cs }

instance HasProp Compact (Group ms) where
    type Prop Compact (Group ms) = Bool
    getProp _ = compact
    setProp _ c sg = sg { compact = c }

instance HasProp Horizontal (Group ms) where
    type Prop Horizontal (Group ms) = Bool
    getProp _ = horizontal
    setProp _ h sg = sg { horizontal = h }

instance HasProp Piled (Group ms) where
    type Prop Piled (Group ms) = Bool
    getProp _ = piled
    setProp _ p sg = sg { piled = p }

instance HasProp Raised (Group ms) where
    type Prop Raised (Group ms) = Bool
    getProp _ = raised
    setProp _ r sg = sg { raised = r }

instance HasProp Size (Group ms) where
    type Prop Size (Group ms) = Txt
    getProp _ = size
    setProp _ s sg = sg { size = s }

instance HasProp Stacked (Group ms) where
    type Prop Stacked (Group ms) = Bool
    getProp _ = stacked
    setProp _ s sg = sg { stacked = s }
