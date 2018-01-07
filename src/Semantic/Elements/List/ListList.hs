module Semantic.Elements.List.ListList where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes

data ListList ms = ListList_ 
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (ListList ms) where
    def = (G.to gdef) { as = Div }

pattern ListList :: ListList ms -> View ms
pattern ListList ll = View ll

instance Pure ListList ms where
    render ListList_ {..} =
        let
            proxy =
                case as [] [] of
                    Ul _ _ -> False
                    Ol _ _ -> False
                    _      -> True

        in
            as 
                ( ClassList ( proxy # "list" : classes )
                : attributes
                )
                children

instance HasAsProp (ListList ms) where
    type AsProp (ListList ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f ll = ll { as = f }

instance HasAttributesProp (ListList ms) where
    type Attribute (ListList ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs ll = ll { attributes = cs }

instance HasChildrenProp (ListList ms) where
    type Child (ListList ms) = View ms
    getChildren = children
    setChildren cs ll = ll { children = cs }

instance HasClassesProp (ListList ms) where
    getClasses = classes
    setClasses cs ll = ll { classes = cs }