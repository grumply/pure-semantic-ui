module Semantic.Views.Feed.FeedDate where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data FeedDate ms = FeedDate_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (FeedDate ms) where
    def = (G.to gdef) { as = Div }

pattern FeedDate :: Typeable ms => FeedDate ms -> View ms
pattern FeedDate fd = View fd

instance Typeable ms => Pure FeedDate ms where
    render FeedDate_ {..} =
        let
            cs =
                ( "date"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children
