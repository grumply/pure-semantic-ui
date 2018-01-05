module Semantic.Views.Comment.CommentAvatar where

import GHC.Generics as G
import Pure.View hiding (Src)
import qualified Pure.View as HTML

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Classes
import Semantic.Properties.Src

data CommentAvatar ms = CommentAvatar_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , classes :: [Txt]
    , src :: Txt
    } deriving (Generic)

instance Default (CommentAvatar ms) where
    def = (G.to gdef) { as = Div }

pattern CommentAvatar :: Typeable ms => CommentAvatar ms -> View ms
pattern CommentAvatar ca = View ca

instance Typeable ms => Pure CommentAvatar ms where
    render CommentAvatar_ {..} =
        let
            cs =
                ( "avatar"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                [Img [ HTML.Src src ] []] 

instance HasAsProp (CommentAvatar ms) where
    type AsProp (CommentAvatar ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a ca = ca { as = a }

instance HasAttributesProp (CommentAvatar ms) where
    type Attribute (CommentAvatar ms) = Feature ms
    getAttributes = attributes
    setAttributes as ca = ca { attributes = as }

instance HasClassesProp (CommentAvatar ms) where
    getClasses = classes
    setClasses cs ca = ca { classes = cs }

instance HasSrcProp (CommentAvatar ms) where
    getSrc = src
    setSrc s ca = ca { src = s }