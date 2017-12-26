module Semantic.Elements.Label.LabelDetail where

import GHC.Generics as G
import Pure.View as View

import Semantic.Utils

import Semantic.Extensions.Attributes
import Semantic.Extensions.Children

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

instance HasAttributes (LabelDetail ms) where
    type Attribute (LabelDetail ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs ld = ld { attributes = cs }

instance HasChildren (LabelDetail ms) where
    type Child (LabelDetail ms) = View ms
    getChildren = children
    setChildren cs ld = ld { children = cs }