module Semantic.Views.Feed.FeedMeta where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes

data FeedMeta ms = FeedMeta_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (FeedMeta ms) where
    def = (G.to gdef) { as = Div }

pattern FeedMeta :: FeedMeta ms -> View ms
pattern FeedMeta fm = View fm

instance Pure FeedMeta ms where
    render FeedMeta_ {..} =
        let
            cs =
                ( "meta"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (FeedMeta ms) where
    type AsProp (FeedMeta ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a fm = fm { as = a }

instance HasAttributesProp (FeedMeta ms) where
    type Attribute (FeedMeta ms) = Feature ms
    getAttributes = attributes
    setAttributes as fm = fm { attributes = as }

instance HasChildrenProp (FeedMeta ms) where
    type Child (FeedMeta ms) = View ms
    getChildren = children
    setChildren cs fm = fm { children = cs }

instance HasClassesProp (FeedMeta ms) where
    getClasses = classes
    setClasses cs fm = fm { classes = cs }
