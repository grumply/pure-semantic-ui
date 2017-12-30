module Semantic.Views.Comment.CommentText where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data CommentText ms = CommentText_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (CommentText ms) where
    def = (G.to gdef) { as = A }

pattern CommentText :: Typeable ms => CommentText ms -> View ms
pattern CommentText ct = View ct 

instance Typeable ms => Pure CommentText ms where
    render CommentText_ {..} =
        let
            cs =
                ( "text"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children
