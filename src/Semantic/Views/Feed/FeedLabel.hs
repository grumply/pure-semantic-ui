module Semantic.Views.Feed.FeedLabel where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data FeedLabel ms = FeedLabel_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (FeedLabel ms) where
    def = (G.to gdef) { as = Div }

pattern FeedLabel :: Typeable ms => FeedLabel ms -> View ms
pattern FeedLabel fl = View fl

instance Typeable ms => Pure FeedLabel ms where
    render FeedLabel_ {..} =
        let
            cs =
                ( "label"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children

