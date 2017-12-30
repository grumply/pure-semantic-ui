module Semantic.Views.Feed.FeedContent where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data FeedContent ms = FeedContent_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (FeedContent ms) where
    def = (G.to gdef) { as = Div }

pattern FeedContent :: Typeable ms => FeedContent ms -> View ms
pattern FeedContent fc = View fc

instance Typeable ms => Pure FeedContent ms where
    render FeedContent_ {..} =
        let
            cs =
                ( "content"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children
