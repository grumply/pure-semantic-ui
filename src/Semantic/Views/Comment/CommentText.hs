module Semantic.Views.Comment.CommentText where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes

data CommentText ms = CommentText_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (CommentText ms) where
    def = (G.to gdef) { as = A }

pattern CommentText :: CommentText ms -> View ms
pattern CommentText ct = View ct 

instance Pure CommentText ms where
    render CommentText_ {..} =
        let
            cs =
                ( "text"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (CommentText ms) where
    type AsProp (CommentText ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a ct = ct { as = a }

instance HasAttributesProp (CommentText ms) where
    type Attribute (CommentText ms) = Feature ms
    getAttributes = attributes
    setAttributes as ct = ct { attributes = as }

instance HasChildrenProp (CommentText ms) where
    type Child (CommentText ms) = View ms
    getChildren = children
    setChildren cs ct = ct { children = cs }

instance HasClassesProp (CommentText ms) where
    getClasses = classes
    setClasses cs ct = ct { classes = cs }