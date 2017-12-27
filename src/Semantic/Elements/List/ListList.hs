module Semantic.Elements.List.ListList where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Extensions.Attributes
import Semantic.Extensions.Children
import Semantic.Extensions.Classes

data ListList ms = ListList_ 
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (ListList ms) where
    def = (G.to gdef) { as = Div }

pattern ListList :: Typeable ms => ListList ms -> View ms
pattern ListList ll = View ll

instance Typeable ms => Pure ListList ms where
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

instance HasAttributes (ListList ms) where
    type Attribute (ListList ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs ll = ll { attributes = cs }

instance HasChildren (ListList ms) where
    type Child (ListList ms) = View ms
    getChildren = children
    setChildren cs ll = ll { children = cs }

instance HasClasses (ListList ms) where
    getClasses = classes
    setClasses cs ll = ll { classes = cs }