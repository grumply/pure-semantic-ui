module Semantic.Elements.List.ListContent where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Extensions.Attributes
import Semantic.Extensions.Children
import Semantic.Extensions.Classes

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

instance HasAttributes (ListContent ms) where
    type Attribute (ListContent ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs lc = lc { attributes = cs }

instance HasChildren (ListContent ms) where
    type Child (ListContent ms) = View ms
    getChildren = children
    setChildren cs lc = lc { children = cs }

instance HasClasses (ListContent ms) where
    getClasses = classes
    setClasses cs lc = lc { classes = cs }