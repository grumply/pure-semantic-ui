module Semantic.Reveal
  ( module Properties
  , module Tools
  , Reveal(..), pattern Reveal
  ) where

import GHC.Generics as G
import Pure.View hiding (disabled,active)

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( pattern Animated, Animated(..)
  , pattern As, As(..)
  , pattern Active, Active(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Classes, Classes(..)
  , pattern Disabled, Disabled(..)
  , pattern Instant, Instant(..)
  )

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

data Reveal ms = Reveal_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    , animated :: Txt
    , disabled :: Bool
    , instant :: Bool
    } deriving (Generic)

instance Default (Reveal ms) where
    def = (G.to gdef) { as = Div }

pattern Reveal :: Reveal ms -> View ms
pattern Reveal r = View r

instance Pure Reveal ms where
    render Reveal_ {..} =
        let
            cs =
                ( "ui"
                : animated
                : active # "active"
                : disabled # "disabled"
                : instant # "instant"
                : "reveal"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp Active (Reveal ms) where
    type Prop Active (Reveal ms) = Bool
    getProp _ = active
    setProp _ a r = r { active = a }

instance HasProp Animated (Reveal ms) where
    type Prop Animated (Reveal ms) = Txt
    getProp _ = animated
    setProp _ a r = r { animated = a }

instance HasProp As (Reveal ms) where
    type Prop As (Reveal ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f r = r { as = f }

instance HasProp Attributes (Reveal ms) where
    type Prop Attributes (Reveal ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs r = r { attributes = cs }

instance HasProp Children (Reveal ms) where
    type Prop Children (Reveal ms) = [View ms]
    getProp _ = children
    setProp _ cs r = r { children = cs }

instance HasProp Classes (Reveal ms) where
    type Prop Classes (Reveal ms) = [Txt]
    getProp _ = classes
    setProp _ cs r = r { classes = cs }

instance HasProp Disabled (Reveal ms) where
    type Prop Disabled (Reveal ms) = Bool
    getProp _ = disabled
    setProp _ d r = r { disabled = d }

instance HasProp Instant (Reveal ms) where
    type Prop Instant (Reveal ms) = Bool
    getProp _ = instant
    setProp _ i r = r { instant = i }
