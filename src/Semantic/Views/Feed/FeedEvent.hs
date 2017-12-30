module Semantic.Views.Feed.FeedEvent where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data FeedEvent ms = FeedEvent_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (FeedEvent ms) where
    def = (G.to gdef) { as = Div }

pattern FeedEvent :: Typeable ms => FeedEvent ms -> View ms
pattern FeedEvent fe = View fe

instance Typeable ms => Pure FeedEvent ms where
    render FeedEvent_ {..} =
        let
            cs =
                ( "event"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children
