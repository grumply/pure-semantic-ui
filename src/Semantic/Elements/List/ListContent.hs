module Semantic.Elements.List.ListContent where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes

data ListContent ms = ListContent_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , floated :: Txt
    , verticalAlign :: Txt
    } deriving (Generic)

instance Default (ListContent ms) where
    def = (G.to gdef) { as = Div }

pattern ListContent :: Typeable ms => ListContent ms -> View ms
pattern ListContent lc = View lc

instance Typeable ms => Pure ListContent ms where
    render ListContent_ {..} =
        let
            cs =
                ( floated # (floated <>> "floated")
                : verticalAlign # (verticalAlign <>> "aligned")
                : "content"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children

instance HasAsProp (ListContent ms) where
    type AsProp (ListContent ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f lc = lc { as = f }

instance HasAttributesProp (ListContent ms) where
    type Attribute (ListContent ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs lc = lc { attributes = cs }

instance HasChildrenProp (ListContent ms) where
    type Child (ListContent ms) = View ms
    getChildren = children
    setChildren cs lc = lc { children = cs }

instance HasClassesProp (ListContent ms) where
    getClasses = classes
    setClasses cs lc = lc { classes = cs }