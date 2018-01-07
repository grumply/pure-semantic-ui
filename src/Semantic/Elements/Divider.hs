module Semantic.Elements.Divider where

import GHC.Generics as G
import Pure.View as View hiding (hidden,horizontal,vertical)

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Clearing
import Semantic.Properties.Fitted
import Semantic.Properties.Hidden
import Semantic.Properties.Horizontal
import Semantic.Properties.Inverted
import Semantic.Properties.Section
import Semantic.Properties.Vertical

data Divider ms = Divider_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , clearing :: Bool
    , fitted :: Bool
    , hidden :: Bool
    , horizontal :: Bool
    , inverted :: Bool
    , section :: Bool
    , vertical :: Bool
    } deriving (Generic)

instance Default (Divider ms) where
    def = (G.to gdef) { as = Div }

pattern Divider :: Divider ms -> View ms
pattern Divider d = View d

instance Pure Divider ms where
    render Divider_ {..} =
        let
            cs =
                ( "ui"
                : clearing # "clearing"
                : fitted # "fitted"
                : hidden # "hidden"
                : horizontal # "horizontal"
                : inverted # "inverted"
                : section # "section"
                : vertical # "verical"
                : "divider"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (Divider ms) where
    type AsProp (Divider ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f d = d { as = f }

instance HasAttributesProp (Divider ms) where
    type Attribute (Divider ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs d = d { attributes = cs }

instance HasChildrenProp (Divider ms) where
    type Child (Divider ms) = View ms
    getChildren = children
    setChildren cs d = d { children = cs }

instance HasClassesProp (Divider ms) where
    getClasses = classes
    setClasses cs d = d { classes = cs }

instance HasClearingProp (Divider ms) where
    getClearing = clearing
    setClearing c d = d { clearing = c }

instance HasFittedProp (Divider ms) where
    getFitted = fitted
    setFitted f d = d { fitted = f }

instance HasHiddenProp (Divider ms) where
    getHidden = hidden
    setHidden h d = d { hidden = h }

instance HasHorizontalProp (Divider ms) where
    getHorizontal = horizontal
    setHorizontal h d = d { horizontal = h }

instance HasInvertedProp (Divider ms) where
    getInverted = inverted
    setInverted i d = d { inverted = i }

instance HasSectionProp (Divider ms) where
    getSection = section
    setSection s d = d { section = s }

instance HasVerticalProp (Divider ms) where
    getVertical = vertical
    setVertical v d = d { vertical = v }