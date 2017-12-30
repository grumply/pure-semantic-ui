module Semantic.Views.Comment.CommentAvatar where

import GHC.Generics as G
import Pure.View hiding (Src)
import qualified Pure.View as HTML

import Semantic.Utils

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
                ( ClassList cs
                : attributes
                )
                [Img [ HTML.Src src ] []] 
