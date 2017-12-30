module Semantic.Views.Feed.FeedMeta where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data FeedMeta ms = FeedMeta_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (FeedMeta ms) where
    def = (G.to gdef) { as = Div }

pattern FeedMeta :: Typeable ms => FeedMeta ms -> View ms
pattern FeedMeta fm = View fm

instance Typeable ms => Pure FeedMeta ms where
    render FeedMeta_ {..} =
        let
            cs =
                ( "meta"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children
