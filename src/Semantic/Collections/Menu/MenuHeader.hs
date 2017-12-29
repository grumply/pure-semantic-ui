module Semantic.Collections.Menu.MenuHeader where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes

data MenuHeader ms = MenuHeader_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (MenuHeader ms) where
    def = (G.to gdef) { as = Div }

pattern MenuHeader :: Typeable ms => MenuHeader ms -> View ms
pattern MenuHeader mh = View mh

instance Typeable ms => Pure MenuHeader ms where
    render MenuHeader_ {..} =
        let
            cs =
                ( "header"
                : classes 
                )
        in
            as 
                ( ClassList cs
                : attributes
                )
                children

instance HasAsProp (MenuHeader ms) where
    type AsProp (MenuHeader ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a mh = mh { as = a }

instance HasAttributesProp (MenuHeader ms) where
    type Attribute (MenuHeader ms) = Feature ms
    getAttributes = attributes
    setAttributes as mh = mh { attributes = as }

instance HasChildrenProp (MenuHeader ms) where
    type Child (MenuHeader ms) = View ms
    getChildren = children
    setChildren cs mh = mh { children = cs }

instance HasClassesProp (MenuHeader ms) where
    getClasses = classes
    setClasses cs mh = mh { classes = cs }

