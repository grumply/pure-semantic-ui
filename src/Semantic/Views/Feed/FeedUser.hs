module Semantic.Views.Feed.FeedUser where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  )

data FeedUser ms = FeedUser_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (FeedUser ms) where
    def = (G.to gdef) { as = A }

pattern FeedUser :: FeedUser ms -> View ms
pattern FeedUser fu = View fu

instance Pure FeedUser ms where
    render FeedUser_ {..} =
        let
            cs =
                ( "user"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (FeedUser ms) where
    type AsProp (FeedUser ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a fu = fu { as = a }

instance HasAttributesProp (FeedUser ms) where
    type Attribute (FeedUser ms) = Feature ms
    getAttributes = attributes
    setAttributes as fu = fu { attributes = as }

instance HasChildrenProp (FeedUser ms) where
    type Child (FeedUser ms) = View ms
    getChildren = children
    setChildren cs fu = fu { children = cs }

instance HasClassesProp (FeedUser ms) where
    getClasses = classes
    setClasses cs fu = fu { classes = cs }
