module Semantic.Views.Comment.CommentContent where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

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
                ( ClassList cs
                : attributes
                )
                children
