module Semantic.Views.Feed.FeedLike where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data FeedLike ms = FeedLike_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (FeedLike ms) where
    def = (G.to gdef) { as = Div }

pattern FeedLike :: Typeable ms => FeedLike ms -> View ms
pattern FeedLike fl = View fl

instance Typeable ms => Pure FeedLike ms where
    render FeedLike_ {..} =
        let
            cs =
                ( "like"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children
