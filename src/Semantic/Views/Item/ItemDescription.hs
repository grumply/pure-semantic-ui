module Semantic.Views.Item.ItemDescription where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes

data ItemDescription ms = ItemDescription_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (ItemDescription ms) where
    def = (G.to gdef) { as = Div }

pattern ItemDescription :: Typeable ms => ItemDescription ms -> View ms
pattern ItemDescription id = View id

instance Typeable ms => Pure ItemDescription ms where
    render ItemDescription_ {..} =
        let
            cs =
                ( "description"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (ItemDescription ms) where
    type AsProp (ItemDescription ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a id = id { as = a }

instance HasAttributesProp (ItemDescription ms) where
    type Attribute (ItemDescription ms) = Feature ms
    getAttributes = attributes
    setAttributes as id = id { attributes = as }

instance HasChildrenProp (ItemDescription ms) where
    type Child (ItemDescription ms) = View ms
    getChildren = children
    setChildren cs id = id { children = cs }

instance HasClassesProp (ItemDescription ms) where
    getClasses = classes
    setClasses cs id = id { classes = cs }
