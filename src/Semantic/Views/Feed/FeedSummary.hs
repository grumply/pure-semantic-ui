module Semantic.Views.Feed.FeedSummary where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes

data FeedSummary ms = FeedSummary_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (FeedSummary ms) where
    def = (G.to gdef) { as = Div }

pattern FeedSummary :: FeedSummary ms -> View ms
pattern FeedSummary fs = View fs

instance Pure FeedSummary ms where
    render FeedSummary_ {..} =
        let
            cs =
                ( "summary"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (FeedSummary ms) where
    type AsProp (FeedSummary ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a fs = fs { as = a }

instance HasAttributesProp (FeedSummary ms) where
    type Attribute (FeedSummary ms) = Feature ms
    getAttributes = attributes
    setAttributes as fs = fs { attributes = as }

instance HasChildrenProp (FeedSummary ms) where
    type Child (FeedSummary ms) = View ms
    getChildren = children
    setChildren cs fs = fs { children = cs }

instance HasClassesProp (FeedSummary ms) where
    getClasses = classes
    setClasses cs fs = fs { classes = cs }
