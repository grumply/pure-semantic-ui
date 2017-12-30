module Semantic.Views.Comment (module Semantic.Views.Comment, module Export) where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data Comment ms = Comment_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , collapsed :: Bool
    } deriving (Generic)

instance Default (Comment ms) where
    def = (G.to gdef) { as = Div }

pattern Comment :: Typeable ms => Comment ms -> View ms
pattern Comment a = View a

instance Typeable ms => Pure Comment ms where
    render Comment_ {..} =
        let
            cs =
                ( collapsed # "collapsed"
                : "comment"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children
