module Semantic.Elements.Label.LabelDetail where

import GHC.Generics as G
import Pure.View as View

import Semantic.Utils

import Semantic.Extensions.As
import Semantic.Extensions.Attributes
import Semantic.Extensions.Children
import Semantic.Extensions.Classes

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

instance HasAs (LabelDetail ms) where
    type Constructor (LabelDetail ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f ld = ld { as = f }

instance HasAttributes (LabelDetail ms) where
    type Attribute (LabelDetail ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs ld = ld { attributes = cs }

instance HasChildren (LabelDetail ms) where
    type Child (LabelDetail ms) = View ms
    getChildren = children
    setChildren cs ld = ld { children = cs }

instance HasClasses (LabelDetail ms) where
    getClasses = classes
    setClasses cs ld = ld { classes = cs }