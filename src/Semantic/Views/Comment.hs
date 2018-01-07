module Semantic.Views.Comment (module Semantic.Views.Comment, module Export) where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Collapsed

import Semantic.Views.Comment.CommentAction as Export
import Semantic.Views.Comment.CommentActions as Export
import Semantic.Views.Comment.CommentAuthor as Export
import Semantic.Views.Comment.CommentAvatar as Export
import Semantic.Views.Comment.CommentContent as Export
import Semantic.Views.Comment.CommentGroup as Export
import Semantic.Views.Comment.CommentMetadata as Export
import Semantic.Views.Comment.CommentText as Export

data Comment ms = Comment_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , collapsed :: Bool
    } deriving (Generic)

instance Default (Comment ms) where
    def = (G.to gdef) { as = Div }

pattern Comment :: Comment ms -> View ms
pattern Comment a = View a

instance Pure Comment ms where
    render Comment_ {..} =
        let
            cs =
                ( collapsed # "collapsed"
                : "comment"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (Comment ms) where
    type AsProp (Comment ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a c = c { as = a }

instance HasAttributesProp (Comment ms) where
    type Attribute (Comment ms) = Feature ms
    getAttributes = attributes
    setAttributes as c = c { attributes = as }

instance HasChildrenProp (Comment ms) where
    type Child (Comment ms) = View ms
    getChildren = children
    setChildren cs c = c { children = cs }

instance HasClassesProp (Comment ms) where
    getClasses = classes
    setClasses cs c = c { classes = cs }

instance HasCollapsedProp (Comment ms) where
    getCollapsed = collapsed
    setCollapsed c com = com { collapsed = c }
   