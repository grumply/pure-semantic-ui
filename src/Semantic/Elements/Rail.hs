module Semantic.Elements.Rail
  ( module Properties
  , module Tools
  , Rail(..), pattern Rail
  ) where

import GHC.Generics as G
import Pure.View hiding (position,verticalAlign)

import Semantic.Utils

import Semantic.Properties as Tools ( (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttachedProp(..), pattern Attached
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  , HasCloseProp(..), pattern Close
  , HasDividingProp(..), pattern Dividing
  , HasInternalProp(..), pattern Internal
  , HasPositionProp(..), pattern Position
  , HasSizeProp(..), pattern Size
  )

data Rail ms = Rail_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , attached :: Bool
    , close :: Maybe Txt
    , dividing :: Bool
    , internal :: Bool
    , position :: Txt
    , size :: Txt
    } deriving (Generic)

instance Default (Rail ms) where
    def = (G.to gdef) { as = Div }

pattern Rail :: Rail ms -> View ms
pattern Rail r = View r

instance Pure Rail ms where
    render Rail_ {..} =
        let
            cs =
                ( "ui"
                : position
                : size
                : attached # "attached"
                : dividing # "dividing"
                : internal # "internal"
                : may (<>> "close") close
                : "rail"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (Rail ms) where
    type AsProp (Rail ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f r = r { as = f }

instance HasAttachedProp (Rail ms) where
    type AttachedProp (Rail ms) = Bool
    getAttached = attached
    setAttached attach r = r { attached = attach }

instance HasAttributesProp (Rail ms) where
    type Attribute (Rail ms) = Feature ms
    getAttributes = attributes
    setAttributes cs r = r { attributes = cs }

instance HasChildrenProp (Rail ms) where
    type Child (Rail ms) = View ms
    getChildren = children
    setChildren cs r = r { children = cs }

instance HasClassesProp (Rail ms) where
    getClasses = classes
    setClasses cs r = r { classes = cs }

instance HasCloseProp (Rail ms) where
    getClose = close
    setClose c r = r { close = c }

instance HasDividingProp (Rail ms) where
    getDividing = dividing
    setDividing d r = r { dividing = d }

instance HasInternalProp (Rail ms) where
    getInternal = internal
    setInternal i r = r { internal = i }

instance HasPositionProp (Rail ms) where
    getPosition = position
    setPosition p r = r { position = p }

instance HasSizeProp (Rail ms) where
    getSize = size
    setSize s r = r { size = s }
