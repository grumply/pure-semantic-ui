module Semantic.Views.Comment.CommentGroup where

import GHC.Generics as G
import Pure.View hiding (textAlign)

import Semantic.Utils

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
                ( ClassList cs
                : attributes
                )
                children
