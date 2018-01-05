module Semantic.Views.Comment.CommentGroup where

import GHC.Generics as G
import Pure.View hiding (textAlign)

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Collapsed
import Semantic.Properties.Minimal
import Semantic.Properties.Size
import Semantic.Properties.Threaded

data CommentGroup ms = CommentGroup_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , collapsed :: Bool
    , minimal :: Bool
    , size :: Txt
    , threaded :: Bool
    } deriving (Generic)

instance Default (CommentGroup ms) where
    def = (G.to gdef) { as = Div }

pattern CommentGroup :: Typeable ms => CommentGroup ms -> View ms
pattern CommentGroup cg = View cg

instance Typeable ms => Pure CommentGroup ms where
    render CommentGroup_ {..} =
        let
            cs =
                ( "ui"
                : size
                : collapsed # "collapsed"
                : minimal # "minimal"
                : threaded # "threaded"
                : "comments"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (CommentGroup ms) where
    type AsProp (CommentGroup ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a cg = cg { as = a }

instance HasAttributesProp (CommentGroup ms) where
    type Attribute (CommentGroup ms) = Feature ms
    getAttributes = attributes
    setAttributes as cg = cg { attributes = as }

instance HasChildrenProp (CommentGroup ms) where
    type Child (CommentGroup ms) = View ms
    getChildren = children
    setChildren cs cg = cg { children = cs }

instance HasClassesProp (CommentGroup ms) where
    getClasses = classes
    setClasses cs cg = cg { classes = cs }

instance HasCollapsedProp (CommentGroup ms) where
    getCollapsed = collapsed
    setCollapsed c cg = cg { collapsed = c }

instance HasMinimalProp (CommentGroup ms) where
    getMinimal = minimal
    setMinimal m cg = cg { minimal = m }

instance HasSizeProp (CommentGroup ms) where
    getSize = size
    setSize s cg = cg { size = s }

instance HasThreadedProp (CommentGroup ms) where
    getThreaded = threaded
    setThreaded t cg = cg { threaded = t }