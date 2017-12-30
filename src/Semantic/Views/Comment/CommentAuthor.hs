module Semantic.Views.Comment.CommentAuthor where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data CommentAuthor ms = CommentAuthor_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (CommentAuthor ms) where
    def = (G.to gdef) { as = Div }

pattern CommentAuthor :: Typeable ms => CommentAuthor ms -> View ms
pattern CommentAuthor ca = View ca

instance Typeable ms => Pure CommentAuthor ms where
    render CommentAuthor_ {..} =
        let
            cs =
                ( "author"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children
