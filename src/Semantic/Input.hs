module Semantic.Input
  ( module Properties
  , module Tools
  , Input(..), pattern Semantic.Input.Input
  ) where

import Pure hiding (transparent,size,disabled,focus,(#))

import GHC.Generics as G

import Semantic.Utils

import Semantic.Button
import Semantic.Icon
import Semantic.Label

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Disabled, Disabled(..)
  , pattern Error, Error(..)
  , pattern Fluid, Fluid(..)
  , pattern Focus, Focus(..)
  , pattern Inverted, Inverted(..)
  , pattern Loading, Loading(..)
  , pattern Size, Size(..)
  , pattern Transparent, Transparent(..)
  )

import Prelude hiding (error)

import Data.Function as Tools ((&))

data Input = Input_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , disabled :: Bool
    , error :: Bool
    , fluid :: Bool
    , focus :: Bool
    , inverted :: Bool
    , loading :: Bool
    , size :: Txt
    , transparent :: Bool
    } deriving (Generic)

instance Default Input where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Input :: Input -> Input
pattern Input i = i

instance Pure Input where
    view Input_ {..} =
        let
            cs =
                [ "ui"
                , size
                , disabled # "disabled"
                , error # "error"
                , fluid # "fluid"
                , focus # "focus"
                , inverted # "inverted"
                , loading # "loading"
                , transparent # "transparent"
                , "input"
                ]
        in
            as (features & Classes cs) children

instance HasProp As Input where
    type Prop As Input = Features -> [View] -> View
    getProp _ = as
    setProp _ f i = i { as = f }

instance HasFeatures Input where
    getFeatures = features
    setFeatures cs i = i { features = cs }

instance HasChildren Input where
    getChildren = children
    setChildren cs i = i { children = cs }

instance HasProp Disabled Input where
    type Prop Disabled Input = Bool
    getProp _ = disabled
    setProp _ d i = i { disabled = d }

instance HasProp Error Input where
    type Prop Error Input = Bool
    getProp _ = error
    setProp _ e i = i { error = e }

instance HasProp Fluid Input where
    type Prop Fluid Input = Bool
    getProp _ = fluid
    setProp _ f i = i { fluid = f }

instance HasProp Focus Input where
    type Prop Focus Input = Bool
    getProp _ = focus
    setProp _ f i = i { focus = f }

instance HasProp Inverted Input where
    type Prop Inverted Input = Bool
    getProp _ = inverted
    setProp _ inv i = i { inverted = inv }

instance HasProp Loading Input where
    type Prop Loading Input = Bool
    getProp _ = loading
    setProp _ l i = i { loading = l }

instance HasProp Size Input where
    type Prop Size Input = Txt
    getProp _ = size
    setProp _ s i = i { size = s }

instance HasProp Transparent Input where
    type Prop Transparent Input = Bool
    getProp _ = transparent
    setProp _ t i = i { transparent = t }
