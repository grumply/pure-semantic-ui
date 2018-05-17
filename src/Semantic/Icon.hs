module Semantic.Icon
  ( module Properties
  , module Tools
  , Icon(..), pattern Icon
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
  , pattern Bordered, Bordered(..)
  , pattern Circular, Circular(..)
  , pattern Color, Color(..)
  , pattern Corner, Corner(..)
  , pattern Disabled, Disabled(..)
  , pattern Fitted, Fitted(..)
  , pattern Flipped, Flipped(..)
  , pattern Inverted, Inverted(..)
  , pattern Link, Link(..)
  , pattern Loading, Loading(..)
  , pattern Name, Name(..)
  , pattern Rotated, Rotated(..)
  , pattern Size, Size(..)
  )

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

data Icon = Icon_
    { as :: Features -> View
    , features :: Features
    , bordered :: Bool
    , circular :: Bool
    , color :: Txt
    , corner :: Bool
    , disabled :: Bool
    , fitted :: Bool
    , flipped :: Txt
    , inverted :: Bool
    , link :: Bool
    , loading :: Bool
    , name :: Txt
    , rotated :: Txt
    , size :: Txt
    } deriving (Generic)

instance Default Icon where
    def = (G.to gdef) { as = flip Features I }

pattern Icon :: Icon -> Icon
pattern Icon i = i

instance Pure Icon where
    view Icon_ {..} =
        let
            cs =
                [ color
                , name
                , size
                , bordered # "bordered"
                , circular # "circular"
                , corner # "corner"
                , disabled # "disabled"
                , fitted # "fitted"
                , inverted # "inverted"
                , link # "link"
                , loading # "loading"
                , (flipped /= def) # ("flipped" <<>> flipped)
                , (rotated /= def) # ("rotated" <<>> rotated)
                , "icon"
                ]
        in

instance HasFeatures Icon where
    getFeatures = features
    setFeatures fs i = i { features = fs }

instance HasProp As Icon where
    type Prop As Icon = Features -> View
    getProp _ = as
    setProp _ f i = i { as = f }

instance HasProp Bordered Icon where
    type Prop Bordered Icon = Bool
    getProp _ = bordered
    setProp _ b i = i { bordered = b }

instance HasProp Circular Icon where
    type Prop Circular Icon = Bool
    getProp _ = circular
    setProp _ c i = i { circular = c }

instance HasProp Name Icon where
    type Prop Name Icon = Txt
    getProp _ = name
    setProp _ n i = i { name = n }

instance HasProp Color Icon where
    type Prop Color Icon = Txt
    getProp _ = color
    setProp _ c i = i { color = c }

instance HasProp Corner Icon where
    type Prop Corner Icon = Bool
    getProp _ = corner
    setProp _ c i = i { corner = c }

instance HasProp Disabled Icon where
    type Prop Disabled Icon = Bool
    getProp _ = disabled
    setProp _ d i = i { disabled = d }

instance HasProp Fitted Icon where
    type Prop Fitted Icon = Bool
    getProp _ = fitted
    setProp _ f i = i { fitted = f }

instance HasProp Flipped Icon where
    type Prop Flipped Icon = Txt
    getProp _ = flipped
    setProp _ f i = i { flipped = f }

instance HasProp Inverted Icon where
    type Prop Inverted Icon = Bool
    getProp _ = inverted
    setProp _ inv i = i { inverted = inv }

instance HasProp Link Icon where
    type Prop Link Icon = Bool
    getProp _ = link
    setProp _ l i = i { link = l }

instance HasProp Loading Icon where
    type Prop Loading Icon = Bool
    getProp _ = loading
    setProp _ l i = i { loading = l }

instance HasProp Rotated Icon where
    type Prop Rotated Icon = Txt
    getProp _ = rotated
    setProp _ r i = i { rotated = r }

instance HasProp Size Icon where
    type Prop Size Icon = Txt
    getProp _ = size
    setProp _ s i = i { size = s }

data Group = Group_
    { as :: Features -> [View] -> View
    , children :: [View]
    , features :: Features
    , size :: Txt
    } deriving (Generic)

instance Default Group where
    def = (G.to gdef) { as = \fs cs -> I & Features fs & Children cs }

pattern Group :: Group -> Group
pattern Group ig = ig

instance Pure Group where
    view Group_ {..} =
        let
            cs =
                [ size
                , "icons"
                ]


instance HasFeatures Group where
    getFeatures = features
    setFeatures fs ig = ig { features = fs }

instance HasChildren Group where
    getChildren = children
    setChildren cs ig = ig { children = cs }

instance HasProp As Group where
    type Prop As Group = Features -> [View] -> View
    getProp _ = as
    setProp _ f ig = ig { as = f }

instance HasProp Size Group where
    type Prop Size Group = Txt
    getProp _ = size
    setProp _ s ig = ig { size = s }
