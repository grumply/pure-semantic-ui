module Semantic.Views.Feed.FeedDate where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  )

data FeedDate ms = FeedDate_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (FeedDate ms) where
    def = (G.to gdef) { as = Div }

pattern FeedDate :: FeedDate ms -> View ms
pattern FeedDate fd = View fd

instance Pure FeedDate ms where
    render FeedDate_ {..} =
        let
            cs =
                ( "date"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (FeedDate ms) where
    type AsProp (FeedDate ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a fd = fd { as = a }

instance HasAttributesProp (FeedDate ms) where
    type Attribute (FeedDate ms) = Feature ms
    getAttributes = attributes
    setAttributes as fd = fd { attributes = as }

instance HasChildrenProp (FeedDate ms) where
    type Child (FeedDate ms) = View ms
    getChildren = children
    setChildren cs fd = fd { children = cs }

instance HasClassesProp (FeedDate ms) where
    getClasses = classes
    setClasses cs fd = fd { classes = cs }
