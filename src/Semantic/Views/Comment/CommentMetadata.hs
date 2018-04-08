module Semantic.Views.Comment.CommentMetadata where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  )

data CommentMetadata ms = CommentMetadata_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (CommentMetadata ms) where
    def = (G.to gdef) { as = A }

pattern CommentMetadata :: CommentMetadata ms -> View ms
pattern CommentMetadata cm = View cm

instance Pure CommentMetadata ms where
    render CommentMetadata_ {..} =
        let
            cs =
                ( "metadata"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (CommentMetadata ms) where
    type AsProp (CommentMetadata ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a cm = cm { as = a }

instance HasAttributesProp (CommentMetadata ms) where
    type Attribute (CommentMetadata ms) = Feature ms
    getAttributes = attributes
    setAttributes as cm = cm { attributes = as }

instance HasChildrenProp (CommentMetadata ms) where
    type Child (CommentMetadata ms) = View ms
    getChildren = children
    setChildren cs cm = cm { children = cs }

instance HasClassesProp (CommentMetadata ms) where
    getClasses = classes
    setClasses cs cm = cm { classes = cs }
