module Semantic.Views.Feed.FeedUser where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data FeedUser ms = FeedUser_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (FeedUser ms) where
    def = (G.to gdef) { as = A }

pattern FeedUser :: Typeable ms => FeedUser ms -> View ms
pattern FeedUser fu = View fu

instance Typeable ms => Pure FeedUser ms where
    render FeedUser_ {..} =
        let
            cs =
                ( "user"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children
