module Semantic.Views.Comment.CommentMetadata where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data CommentMetadata ms = CommentMetadata_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (CommentMetadata ms) where
    def = (G.to gdef) { as = A }

pattern CommentMetadata :: Typeable ms => CommentMetadata ms -> View ms
pattern CommentMetadata cm = View cm

instance Typeable ms => Pure CommentMetadata ms where
    render CommentMetadata_ {..} =
        let
            cs =
                ( "metadata"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children
