module Semantic.Views.Comment.CommentActions where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data CommentActions ms = CommentActions_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (CommentActions ms) where
    def = (G.to gdef) { as = Div }

pattern CommentActions :: Typeable ms => CommentActions ms -> View ms
pattern CommentActions ca = View ca

instance Typeable ms => Pure CommentActions ms where
    render CommentActions_ {..} =
        let
            cs =
                ( "Actionss"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children
