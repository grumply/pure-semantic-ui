module Semantic.Views.Feed.FeedLike where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes

data FeedLike ms = FeedLike_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (FeedLike ms) where
    def = (G.to gdef) { as = Div }

pattern FeedLike :: Typeable ms => FeedLike ms -> View ms
pattern FeedLike fl = View fl

instance Typeable ms => Pure FeedLike ms where
    render FeedLike_ {..} =
        let
            cs =
                ( "like"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (FeedLike ms) where
    type AsProp (FeedLike ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a fl = fl { as = a }

instance HasAttributesProp (FeedLike ms) where
    type Attribute (FeedLike ms) = Feature ms
    getAttributes = attributes
    setAttributes as fl = fl { attributes = as }

instance HasChildrenProp (FeedLike ms) where
    type Child (FeedLike ms) = View ms
    getChildren = children
    setChildren cs fl = fl { children = cs }

instance HasClassesProp (FeedLike ms) where
    getClasses = classes
    setClasses cs fl = fl { classes = cs }
