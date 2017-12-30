module Semantic.Views.Feed.FeedSummary where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data FeedSummary ms = FeedSummary_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (FeedSummary ms) where
    def = (G.to gdef) { as = Div }

pattern FeedSummary :: Typeable ms => FeedSummary ms -> View ms
pattern FeedSummary fs = View fs

instance Typeable ms => Pure FeedSummary ms where
    render FeedSummary_ {..} =
        let
            cs =
                ( "summary"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children
