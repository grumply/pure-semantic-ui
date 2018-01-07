module Semantic.Views.Comment.CommentActions where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes

data CommentActions ms = CommentActions_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (CommentActions ms) where
    def = (G.to gdef) { as = Div }

pattern CommentActions :: CommentActions ms -> View ms
pattern CommentActions ca = View ca

instance Pure CommentActions ms where
    render CommentActions_ {..} =
        let
            cs =
                ( "Actionss"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (CommentActions ms) where
    type AsProp (CommentActions ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a ca = ca { as = a }

instance HasAttributesProp (CommentActions ms) where
    type Attribute (CommentActions ms) = Feature ms
    getAttributes = attributes
    setAttributes as ca = ca { attributes = as }

instance HasChildrenProp (CommentActions ms) where
    type Child (CommentActions ms) = View ms
    getChildren = children
    setChildren cs ca = ca { children = cs }

instance HasClassesProp (CommentActions ms) where
    getClasses = classes
    setClasses cs ca = ca { classes = cs }
