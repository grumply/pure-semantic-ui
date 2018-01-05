module Semantic.Views.Comment.CommentContent where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes

data CommentContent ms = CommentContent_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (CommentContent ms) where
    def = (G.to gdef) { as = Div }

pattern CommentContent :: Typeable ms => CommentContent ms -> View ms
pattern CommentContent cc = View cc

instance Typeable ms => Pure CommentContent ms where
    render CommentContent_ {..} =
        let
            cs =
                ( "content"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (CommentContent ms) where
    type AsProp (CommentContent ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a cc = cc { as = a }

instance HasAttributesProp (CommentContent ms) where
    type Attribute (CommentContent ms) = Feature ms
    getAttributes = attributes
    setAttributes as cc = cc { attributes = as }

instance HasChildrenProp (CommentContent ms) where
    type Child (CommentContent ms) = View ms
    getChildren = children
    setChildren cs cc = cc { children = cs }

instance HasClassesProp (CommentContent ms) where
    getClasses = classes
    setClasses cs cc = cc { classes = cs }
