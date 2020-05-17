module Semantic.Header
  ( module Properties
  , module Tools
  , Header(..), pattern Header
  , Content(..), pattern Content
  , Subheader(..), pattern Subheader
  ) where

import Pure hiding (Content,Content_,Header,block,color,size,disabled,link,active,(#))

import GHC.Generics as G (Generic,to)

import Semantic.Utils

import Semantic.Icon
import Semantic.Image

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attached, Attached(..)
  , pattern Block, Block(..)
  , pattern Color, Color(..)
  , pattern Disabled, Disabled(..)
  , pattern Dividing, Dividing(..)
  , pattern Floated, Floated(..)
  , pattern Inverted, Inverted(..)
  , pattern Size, Size(..)
  , pattern Sub, Sub(..)
  , pattern TextAlign, TextAlign(..)
  )

import Data.Function as Tools ((&))

data Header = Header_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , attached :: Maybe Txt
    , block :: Bool
    , color :: Txt
    , disabled :: Bool
    , dividing :: Bool
    , floated :: Txt
    , icon :: Bool
    , image :: Bool
    , inverted :: Bool
    , size :: Txt
    , sub :: Bool
    , textAlign :: Txt
    } deriving (Generic)

instance Default Header where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Header :: Header -> Header
pattern Header h = h

instance Pure Header where
    view Header_ {..} =
        let
            cs =
                [ "ui"
                , color
                , size
                , block # "block"
                , disabled # "disabled"
                , dividing # "dividing"
                , (floated /= def) # (floated <<>> "floated")
                , icon # "icon"
                , image # "image"
                , inverted # "inverted"
                , sub # "sub"
                , maybe def (<>> "attached") attached
                , textAlign
                , "header"
                ]
        in
            as (features & Classes cs) children

instance HasProp As Header where
    type Prop As Header = Features -> [View] -> View
    getProp _ = as
    setProp _ f h = h { as = f }

instance HasProp Attached Header where
    type Prop Attached Header = Maybe Txt
    getProp _ = attached
    setProp _ attach h = h { attached = attach }

instance HasFeatures Header where
    getFeatures = features
    setFeatures fs h = h { features = fs }

instance HasProp Block Header where
    type Prop Block Header = Bool
    getProp _ = block
    setProp _ b h = h { block = b }

instance HasChildren Header where
    getChildren = children
    setChildren cs h = h { children = cs }

instance HasProp Color Header where
    type Prop Color Header = Txt
    getProp _ = color
    setProp _ c h = h { color = c }

instance HasProp Disabled Header where
    type Prop Disabled Header = Bool
    getProp _ = disabled
    setProp _ d h = h { disabled = d }

instance HasProp Dividing Header where
    type Prop Dividing Header = Bool
    getProp _ = dividing
    setProp _ d h = h { dividing = d }

instance HasProp Floated Header where
    type Prop Floated Header = Txt
    getProp _ = floated
    setProp _ f h = h { floated = f }

instance HasProp Inverted Header where
    type Prop Inverted Header = Bool
    getProp _ = inverted
    setProp _ i h = h { inverted = i }

instance HasProp Size Header where
    type Prop Size Header = Txt
    getProp _ = size
    setProp _ s h = h { size = s }

instance HasProp Sub Header where
    type Prop Sub Header = Bool
    getProp _ = sub
    setProp _ s h = h { sub = s }

instance HasProp TextAlign Header where
    type Prop TextAlign Header = Txt
    getProp _ = textAlign
    setProp _ ta h = h { textAlign = ta }

data Content = Content_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Content where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Content :: Content -> Content
pattern Content hc = hc

instance Pure Content where
    view Content_ {..} = as (features & Class "content") children

instance HasProp As Content where
    type Prop As Content = Features -> [View] -> View
    getProp _ = as
    setProp _ f hc = hc { as = f }

instance HasFeatures Content where
    getFeatures = features
    setFeatures fs hc = hc { features = fs }

instance HasChildren Content where
    getChildren = children
    setChildren cs hc = hc { children = cs }

data Subheader = Subheader_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Subheader where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Subheader :: Subheader -> Subheader
pattern Subheader hs = hs

instance Pure Subheader where
    view Subheader_ {..} = as (features & Classes ["sub","header"]) children

instance HasProp As Subheader where
    type Prop As Subheader = Features -> [View] -> View
    getProp _ = as
    setProp _ f hs = hs { as = f }

instance HasFeatures Subheader where
    getFeatures = features
    setFeatures fs hs = hs { features = fs }

instance HasChildren Subheader where
    getChildren = children
    setChildren cs hs = hs { children = cs }
