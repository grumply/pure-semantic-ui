module Semantic.Elements.Label.LabelDetail where

import GHC.Generics as G
import Pure.View as View

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes

data LabelDetail ms = LabelDetail_ 
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (LabelDetail ms) where
    def = G.to gdef

pattern LabelDetail :: Typeable ms => LabelDetail ms -> View ms
pattern LabelDetail ld = View ld

instance Typeable ms => Pure LabelDetail ms where
    render LabelDetail_ {..} =
        as 
            ( ClassList ("detail" : classes)
            : attributes
            )
            children

instance HasAsProp (LabelDetail ms) where
    type AsProp (LabelDetail ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f ld = ld { as = f }

instance HasAttributesProp (LabelDetail ms) where
    type Attribute (LabelDetail ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs ld = ld { attributes = cs }

instance HasChildrenProp (LabelDetail ms) where
    type Child (LabelDetail ms) = View ms
    getChildren = children
    setChildren cs ld = ld { children = cs }

instance HasClassesProp (LabelDetail ms) where
    getClasses = classes
    setClasses cs ld = ld { classes = cs }