module Semantic.Segment
  ( module Properties
  , module Tools
  , Segment(..), pattern Segment
  , Group(..), pattern Group
  ) where

import Pure hiding (color,vertical,horizontal,size,disabled,(#))

import GHC.Generics as G

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attached, Attached(..)
  , pattern Basic, Basic(..)
  , pattern Circular, Circular(..)
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

data Segment = Segment_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
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

instance Default Segment where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Segment :: Segment -> Segment
pattern Segment s = s

instance Pure Segment where
    view Segment_ {..} =
        let
            cs =
                [ "ui"
                , color
                , size
                , basic # "basic"
                , circular # "circular"
                , clearing # "clearing"
                , compact # "compact"
                , disabled # "disabled"
                , inverted # "inverted"
                , loading # "loading"
                , piled # "piled"
                , raised # "raised"
                , secondary # "secondary"
                , stacked # "stacked"
                , tertiary # "tertiary"
                , vertical # "vertical"
                , (attached /= mempty) # (attached <>> "attached")
                , maybe "" (<>> "padded") padded
                , textAlign
                , (floated /= mempty) # ("floated" <<>> floated)
                , "segment"
                ]
        in
            as (features & Classes cs) children

instance HasProp As Segment where
    type Prop As Segment = Features -> [View] -> View
    getProp _ = as
    setProp _ a s = s { as = a }

instance HasProp Attached Segment where
    type Prop Attached Segment = Txt
    getProp _ = attached
    setProp _ a s = s { attached = a }

instance HasFeatures Segment where
    getFeatures = features
    setFeatures as s = s { features = as }

instance HasProp Basic Segment where
    type Prop Basic Segment = Bool
    getProp _ = basic
    setProp _ b s = s { basic = b }

instance HasChildren Segment where
    getChildren = children
    setChildren cs s = s { children = cs }

instance HasProp Circular Segment where
    type Prop Circular Segment = Bool
    getProp _ = circular
    setProp _ c s = s { circular = c }

instance HasProp Clearing Segment where
    type Prop Clearing Segment = Bool
    getProp _ = clearing
    setProp _ c s = s { clearing = c }

instance HasProp Color Segment where
    type Prop Color Segment = Txt
    getProp _ = color
    setProp _ c s = s { color = c }

instance HasProp Compact Segment where
    type Prop Compact Segment = Bool
    getProp _ = compact
    setProp _ c s = s { compact = c }

instance HasProp Disabled Segment where
    type Prop Disabled Segment = Bool
    getProp _ = disabled
    setProp _ d s = s { disabled = d }

instance HasProp Floated Segment where
    type Prop Floated Segment = Txt
    getProp _ = floated
    setProp _ f s = s { floated = f }

instance HasProp Inverted Segment where
    type Prop Inverted Segment = Bool
    getProp _ = inverted
    setProp _ i s = s { inverted = i }

instance HasProp Loading Segment where
    type Prop Loading Segment = Bool
    getProp _ = loading
    setProp _ l s = s { loading = l }

instance HasProp Padded Segment where
    type Prop Padded Segment = Maybe Txt
    getProp _ = padded
    setProp _ p s = s { padded = p }

instance HasProp Piled Segment where
    type Prop Piled Segment = Bool
    getProp _ = piled
    setProp _ p s = s { piled = p }

instance HasProp Raised Segment where
    type Prop Raised Segment = Bool
    getProp _ = raised
    setProp _ r s = s { raised = r }

instance HasProp Secondary Segment where
    type Prop Secondary Segment = Bool
    getProp _ = secondary
    setProp _ sec s = s { secondary = sec }

instance HasProp Size Segment where
    type Prop Size Segment = Txt
    getProp _ = size
    setProp _ sz s = s { size = sz }

instance HasProp Stacked Segment where
    type Prop Stacked Segment = Bool
    getProp _ = stacked
    setProp _ stkd s = s { stacked = stkd }

instance HasProp Tertiary Segment where
    type Prop Tertiary Segment = Bool
    getProp _ = tertiary
    setProp _ t s = s { tertiary = t }

instance HasProp TextAlign Segment where
    type Prop TextAlign Segment = Txt
    getProp _ = textAlign
    setProp _ ta s = s { textAlign = ta }

instance HasProp Vertical Segment where
    type Prop Vertical Segment = Bool
    getProp _ = vertical
    setProp _ v s = s { vertical = v }

data Group = Group_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , compact :: Bool
    , horizontal :: Bool
    , piled :: Bool
    , raised :: Bool
    , size :: Txt
    , stacked :: Bool
    } deriving (Generic)

instance Default Group where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Group :: Group -> Group
pattern Group sg = sg

instance Pure Group where
    view Group_ {..} =
        let
            cs =
                [ "ui"
                , size
                , compact # "compact"
                , horizontal # "horizontal"
                , piled # "piled"
                , raised # "raised"
                , stacked # "stacked"
                , "segments"
                ]
        in
            as (features & Classes cs) children

instance HasProp As Group where
    type Prop As Group = Features -> [View] -> View
    getProp _ = as
    setProp _ a sg = sg { as = a }

instance HasFeatures Group where
    getFeatures = features
    setFeatures as sg = sg { features = as }

instance HasChildren Group where
    getChildren = children
    setChildren cs sg = sg { children = cs }

instance HasProp Compact Group where
    type Prop Compact Group = Bool
    getProp _ = compact
    setProp _ c sg = sg { compact = c }

instance HasProp Horizontal Group where
    type Prop Horizontal Group = Bool
    getProp _ = horizontal
    setProp _ h sg = sg { horizontal = h }

instance HasProp Piled Group where
    type Prop Piled Group = Bool
    getProp _ = piled
    setProp _ p sg = sg { piled = p }

instance HasProp Raised Group where
    type Prop Raised Group = Bool
    getProp _ = raised
    setProp _ r sg = sg { raised = r }

instance HasProp Size Group where
    type Prop Size Group = Txt
    getProp _ = size
    setProp _ s sg = sg { size = s }

instance HasProp Stacked Group where
    type Prop Stacked Group = Bool
    getProp _ = stacked
    setProp _ s sg = sg { stacked = s }
