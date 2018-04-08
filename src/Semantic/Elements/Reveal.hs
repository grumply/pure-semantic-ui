module Semantic.Elements.Reveal
  ( module Properties
  , module Tools
  , Reveal(..), pattern Reveal
  ) where

import GHC.Generics as G
import Pure.View hiding (disabled)

import Semantic.Utils

import Semantic.Properties as Tools ( (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( HasAnimatedProp(..), pattern Animated
  , HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  , HasDisabledProp(..), pattern Disabled
  , HasInstantProp(..), pattern Instant
  )

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

instance HasAnimatedProp (Reveal ms) where
    type AnimatedProp (Reveal ms) = Txt
    getAnimated = animated
    setAnimated a r = r { animated = a }

instance HasAsProp (Reveal ms) where
    type AsProp (Reveal ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f r = r { as = f }

instance HasAttributesProp (Reveal ms) where
    type Attribute (Reveal ms) = Feature ms
    getAttributes = attributes
    setAttributes cs r = r { attributes = cs }

instance HasChildrenProp (Reveal ms) where
    type Child (Reveal ms) = View ms
    getChildren = children
    setChildren cs r = r { children = cs }

instance HasClassesProp (Reveal ms) where
    getClasses = classes
    setClasses cs r = r { classes = cs }

instance HasDisabledProp (Reveal ms) where
    getDisabled = disabled
    setDisabled d r = r { disabled = d }

instance HasInstantProp (Reveal ms) where
    getInstant = instant
    setInstant i r = r { instant = i }
