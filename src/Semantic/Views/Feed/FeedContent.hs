module Semantic.Views.Feed.FeedContent where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes

data FeedContent ms = FeedContent_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (FeedContent ms) where
    def = (G.to gdef) { as = Div }

pattern FeedContent :: Typeable ms => FeedContent ms -> View ms
pattern FeedContent fc = View fc

instance Typeable ms => Pure FeedContent ms where
    render FeedContent_ {..} =
        let
            cs =
                ( "content"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children

instance HasAsProp (FeedContent ms) where
    type AsProp (FeedContent ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a fc = fc { as = a }

instance HasAttributesProp (FeedContent ms) where
    type Attribute (FeedContent ms) = Feature ms
    getAttributes = attributes
    setAttributes as fc = fc { attributes = as }

instance HasChildrenProp (FeedContent ms) where
    type Child (FeedContent ms) = View ms
    getChildren = children
    setChildren cs fc = fc { children = cs }

instance HasClassesProp (FeedContent ms) where
    getClasses = classes
    setClasses cs fc = fc { classes = cs }
