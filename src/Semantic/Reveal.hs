module Semantic.Reveal
  ( module Properties
  , module Tools
  , Reveal(..), pattern Reveal
  ) where

import GHC.Generics as G
import Pure.Data.View
import Pure.Data.View.Patterns
import Pure.Data.Txt
import Pure.Data.HTML

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern Animated, Animated(..)
  , pattern As, As(..)
  , pattern Active, Active(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Disabled, Disabled(..)
  , pattern Instant, Instant(..)
  )

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

data Reveal = Reveal_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , active :: Bool
    , animated :: Txt
    , disabled :: Bool
    , instant :: Bool
    } deriving (Generic)

instance Default Reveal where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Reveal :: Reveal -> Reveal
pattern Reveal r = r

instance Pure Reveal where
    view Reveal_ {..} =
        let
            cs =
                [ "ui"
                , animated
                , active # "active"
                , disabled # "disabled"
                , instant # "instant"
                , "reveal"
                ]
        in
            as
                : attributes
                )
                children

instance HasProp Active Reveal where
    type Prop Active Reveal = Bool
    getProp _ = active
    setProp _ a r = r { active = a }

instance HasProp Animated Reveal where
    type Prop Animated Reveal = Txt
    getProp _ = animated
    setProp _ a r = r { animated = a }

instance HasProp As Reveal where
    type Prop As Reveal = Features -> [View] -> View
    getProp _ = as
    setProp _ f r = r { as = f }

instance HasFeatures Reveal where
    getFeatures = features
    setFeatures cs r = r { features = cs }

instance HasChildren Reveal where
    getChildren = children
    setChildren cs r = r { children = cs }

instance HasProp Disabled Reveal where
    type Prop Disabled Reveal = Bool
    getProp _ = disabled
    setProp _ d r = r { disabled = d }

instance HasProp Instant Reveal where
    type Prop Instant Reveal = Bool
    getProp _ = instant
    setProp _ i r = r { instant = i }
