module Semantic.Image
  ( module Properties
  , module Tools
  , Image(..), pattern Image
  , Group(..), pattern Group
  ) where

import GHC.Generics as G (Generic,to)
import Pure.Data.View
import Pure.Data.View.Patterns
import Pure.Data.Txt
import Pure.Data.HTML

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Avatar, Avatar(..)
  , pattern Bordered, Bordered(..)
  , pattern Centered, Centered(..)
  , pattern Circular, Circular(..)
  , pattern Disabled, Disabled(..)
  , pattern Floated, Floated(..)
  , pattern Fluid, Fluid(..)
  , pattern Hidden, Hidden(..)
  , pattern Inline, Inline(..)
  , pattern Rounded, Rounded(..)
  , pattern Size, Size(..)
  , pattern Spaced, Spaced(..)
  , pattern UI, UI(..)
  , pattern VerticalAlign, VerticalAlign(..)
  , pattern Wrapped, Wrapped(..)
  )

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

data Image = Image_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , avatar :: Bool
    , bordered :: Bool
    , centered :: Bool
    , circular :: Bool
    , disabled :: Bool
    , floated :: Txt
    , fluid :: Bool
    , hidden :: Bool
    , inline :: Bool
    , rounded :: Bool
    , size :: Txt
    , spaced :: Maybe Txt
    , ui :: Bool
    , verticalAlign :: Txt
    , wrapped :: Bool
    } deriving (Generic)

instance Default Image where
    def = (G.to gdef) { ui = True, as = \fs cs -> Img & Features fs & Children cs }

pattern Image :: Image -> Image
pattern Image i = i

instance Pure Image where
    view Image_ {..} =
        let
            cs =
                [ ui # "ui"
                , size
                , avatar # "avatar"
                , bordered # "bordered"
                , circular # "circular"
                , centered # "centered"
                , disabled # "disabled"
                , fluid # "fluid"
                , hidden # "hidden"
                , inline # "inline"
                , rounded # "rounded"
                , useKeyOrValueAndKey spaced "spaced"
                , (floated /= def) # ("floated" <<>> floated)
                , (verticalAlign /= def) # ("aligned" <<>> verticalAlign)
                , "image"
                ]
        in
            as (features & Classes cs) children

instance HasProp Avatar Image where
    type Prop Avatar Image = Bool
    getProp _ = avatar
    setProp _ a i = i { avatar = a }

instance HasProp As Image where
    type Prop As Image = Features -> [View] -> View
    getProp _ = as
    setProp _ f i = i { as = f }

instance HasFeatures Image where
    getFeatures = features
    setFeatures fs i = i { features = fs }

instance HasProp Bordered Image where
    type Prop Bordered Image = Bool
    getProp _ = bordered
    setProp _ b i = i { bordered = b }

instance HasProp Centered Image where
    type Prop Centered Image = Bool
    getProp _ = centered
    setProp _ c i = i { centered = c }

instance HasChildren Image where
    getChildren = children
    setChildren cs i = i { children = cs }

instance HasProp Circular Image where
    type Prop Circular Image = Bool
    getProp _ = circular
    setProp _ c i = i { circular = c }

instance HasProp Disabled Image where
    type Prop Disabled Image = Bool
    getProp _ = disabled
    setProp _ d i = i { disabled = d }

instance HasProp Floated Image where
    type Prop Floated Image = Txt
    getProp _ = floated
    setProp _ f i = i { floated = f }

instance HasProp Fluid Image where
    type Prop Fluid Image = Bool
    getProp _ = fluid
    setProp _ f i = i { fluid = f }

instance HasProp Inline Image where
    type Prop Inline Image = Bool
    getProp _ = inline
    setProp _ inl i = i { inline = inl }

instance HasProp Hidden Image where
    type Prop Hidden Image = Bool
    getProp _ = hidden
    setProp _ h i = i { hidden = h }

instance HasProp Rounded Image where
    type Prop Rounded Image = Bool
    getProp _ = rounded
    setProp _ r i = i { rounded = r }

instance HasProp Size Image where
    type Prop Size Image = Txt
    getProp _ = size
    setProp _ s i = i { size = s }

instance HasProp Spaced Image where
    type Prop Spaced Image = Maybe Txt
    getProp _ = spaced
    setProp _ s i = i { spaced = s }

instance HasProp UI Image where
    type Prop UI Image = Bool
    getProp _ = ui
    setProp _ x i = i { ui = x }

instance HasProp VerticalAlign Image where
    type Prop VerticalAlign Image = Txt
    getProp _ = verticalAlign
    setProp _ va i = i { verticalAlign = va }

instance HasProp Wrapped Image where
    type Prop Wrapped Image = Bool
    getProp _ = wrapped
    setProp _ w i = i { wrapped = w }

data Group = Group_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , size :: Txt
    } deriving (Generic)

instance Default Group where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Group :: Group -> Group
pattern Group ig = ig

instance Pure Group where
    view Group_ {..} =
        let
            cs =
                [ "ui"
                , size
                , "images"
                ]
        in
            as (features & Classes cs) children

instance HasProp As Group where
    type Prop As Group = Features -> [View] -> View
    getProp _ = as
    setProp _ f ig = ig { as = f }

instance HasFeatures Group where
    getFeatures = features
    setFeatures fs ig = ig { features = fs }

instance HasChildren Group where
    getChildren = children
    setChildren cs ig = ig { children = cs }

instance HasProp Size Group where
    type Prop Size Group = Txt
    getProp _ = size
    setProp _ s ig = ig { size = s }
