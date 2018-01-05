module Semantic.Views.Item.ItemContent where

import GHC.Generics as G
import Pure.View hiding (verticalAlign)

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.VerticalAlign

data ItemContent ms = ItemContent_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , verticalAlign :: Txt
    } deriving (Generic)

instance Default (ItemContent ms) where
    def = (G.to gdef) { as = Div }

pattern ItemContent :: Typeable ms => ItemContent ms -> View ms
pattern ItemContent ic = View ic

instance Typeable ms => Pure ItemContent ms where
    render ItemContent_ {..} =
        let
            cs =
                ( verticalAlign
                : "content"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (ItemContent ms) where
    type AsProp (ItemContent ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a ic = ic { as = a }

instance HasAttributesProp (ItemContent ms) where
    type Attribute (ItemContent ms) = Feature ms
    getAttributes = attributes
    setAttributes as ic = ic { attributes = as }

instance HasChildrenProp (ItemContent ms) where
    type Child (ItemContent ms) = View ms
    getChildren = children
    setChildren cs ic = ic { children = cs }

instance HasClassesProp (ItemContent ms) where
    getClasses = classes
    setClasses cs ic = ic { classes = cs }

instance HasVerticalAlignProp (ItemContent ms) where
    getVerticalAlign = verticalAlign
    setVerticalAlign va ic = ic { verticalAlign = va }
