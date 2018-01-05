module Semantic.Elements.Loader where

import GHC.Generics as G
import Pure.View hiding (active,disabled,inline,verticalAlign)

import Semantic.Utils

import Semantic.Properties.Active
import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Disabled
import Semantic.Properties.Inline
import Semantic.Properties.Inverted
import Semantic.Properties.IsIndeterminate
import Semantic.Properties.Size

data Loader ms = Loader_ 
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    , disabled :: Bool
    , indeterminate :: Bool
    , inline :: Maybe Txt
    , inverted :: Bool
    , size :: Txt
    } deriving (Generic)

instance Default (Loader ms) where
    def = (G.to gdef) { as = Div }

pattern Loader :: Typeable ms => Loader ms -> View ms
pattern Loader l = View l

instance Typeable ms => Pure Loader ms where
    render Loader_ {..} =
        let
            cs =
                ( "ui"
                : size
                : active # "active"
                : disabled # "disabled"
                : indeterminate # "indeterminate"
                : inverted # "inverted"
                : children # "text"
                : may (<>> "inline") inline
                : "loader"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasActiveProp (Loader ms) where
    getActive = active
    setActive a l = l { active = a }

instance HasAsProp (Loader ms) where
    type AsProp (Loader ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f l = l { as = f }

instance HasAttributesProp (Loader ms) where
    type Attribute (Loader ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs l = l { attributes = cs }

instance HasChildrenProp (Loader ms) where
    type Child (Loader ms) = View ms
    getChildren = children
    setChildren cs l = l { children = cs }

instance HasClassesProp (Loader ms) where
    getClasses = classes
    setClasses cs l = l { classes = cs }

instance HasDisabledProp (Loader ms) where
    getDisabled = disabled
    setDisabled d l = l { disabled = d }

instance HasInlineProp (Loader ms) where
    type InlineProp (Loader ms) = Maybe Txt
    getInline = inline
    setInline i l = l { inline = i }

instance HasIsIndeterminateProp (Loader ms) where
    getIsIndeterminate = indeterminate
    setIsIndeterminate i l = l { indeterminate = i }

instance HasInvertedProp (Loader ms) where
    getInverted = inverted
    setInverted i l = l { inverted = i }

instance HasSizeProp (Loader ms) where
    getSize = size
    setSize s l = l { size = s }