module Semantic.Views.Feed (module Semantic.Views.Feed, module Export) where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data Feed ms = Feed_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , size :: Txt
    } deriving (Generic)

instance Default (Feed ms) where
    def = (G.to gdef) { as = Div }

pattern Feed :: Typeable ms => Feed ms -> View ms
pattern Feed f = View f

instance Typeable ms => Pure Feed ms where
    render Feed_ {..} =
        let
            cs =
                ( "ui"
                : size
                : "feed"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children
