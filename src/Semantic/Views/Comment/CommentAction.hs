module Semantic.Views.Comment.CommentAction where

import GHC.Generics as G
import Pure.View hiding (active)

import Semantic.Utils

data CommentAction ms = CommentAction_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    } deriving (Generic)

instance Default (CommentAction ms) where
    def = (G.to gdef) { as = A }

pattern CommentAction :: Typeable ms => CommentAction ms -> View ms
pattern CommentAction ca = View ca

instance Typeable ms => Pure CommentAction ms where
    render CommentAction_ {..} =
        let
            cs =
                ( active # "active"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children
