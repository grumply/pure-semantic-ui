module Semantic.Views.Feed.FeedExtra where

import GHC.Generics as G
import Pure.View hiding (text)

import Semantic.Utils

import Semantic.Elements.Image

data FeedExtra ms = FeedExtra_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , text :: Bool
    } deriving (Generic)

instance Default (FeedExtra ms) where
    def = (G.to gdef) { as = Div }

pattern FeedExtra :: Typeable ms => FeedExtra ms -> View ms
pattern FeedExtra fe = View fe

instance Typeable ms => Pure FeedExtra ms where
    render FeedExtra_ {..} =
        let
            image = foldPures (\(Image_ {}) -> const True) False children

            cs =
                ( image # "images"
                : text # "text"
                : "extra"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children
