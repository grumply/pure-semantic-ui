module Semantic.Views.Feed.FeedEvent where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes

data FeedEvent ms = FeedEvent_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (FeedEvent ms) where
    def = (G.to gdef) { as = Div }

pattern FeedEvent :: FeedEvent ms -> View ms
pattern FeedEvent fe = View fe

instance Pure FeedEvent ms where
    render FeedEvent_ {..} =
        let
            cs =
                ( "event"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (FeedEvent ms) where
    type AsProp (FeedEvent ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a fe = fe { as = a }

instance HasAttributesProp (FeedEvent ms) where
    type Attribute (FeedEvent ms) = Feature ms
    getAttributes = attributes
    setAttributes as fe = fe { attributes = as }

instance HasChildrenProp (FeedEvent ms) where
    type Child (FeedEvent ms) = View ms
    getChildren = children
    setChildren cs fe = fe { children = cs }

instance HasClassesProp (FeedEvent ms) where
    getClasses = classes
    setClasses cs fe = fe { classes = cs }
