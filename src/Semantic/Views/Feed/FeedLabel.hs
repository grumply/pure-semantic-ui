module Semantic.Views.Feed.FeedLabel where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  )

data FeedLabel ms = FeedLabel_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (FeedLabel ms) where
    def = (G.to gdef) { as = Div }

pattern FeedLabel :: FeedLabel ms -> View ms
pattern FeedLabel fl = View fl

instance Pure FeedLabel ms where
    render FeedLabel_ {..} =
        let
            cs =
                ( "label"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (FeedLabel ms) where
    type AsProp (FeedLabel ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a fl = fl { as = a }

instance HasAttributesProp (FeedLabel ms) where
    type Attribute (FeedLabel ms) = Feature ms
    getAttributes = attributes
    setAttributes as fl = fl { attributes = as }

instance HasChildrenProp (FeedLabel ms) where
    type Child (FeedLabel ms) = View ms
    getChildren = children
    setChildren cs fl = fl { children = cs }

instance HasClassesProp (FeedLabel ms) where
    getClasses = classes
    setClasses cs fl = fl { classes = cs }
