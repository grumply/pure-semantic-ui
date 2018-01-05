module Semantic.Elements.Rail where

import GHC.Generics as G
import Pure.View hiding (position,verticalAlign)

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attached
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Close
import Semantic.Properties.Dividing
import Semantic.Properties.Internal
import Semantic.Properties.Position
import Semantic.Properties.Size

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

pattern Rail :: Typeable ms => Rail ms -> View ms
pattern Rail r = View r

instance Typeable ms => Pure Rail ms where
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