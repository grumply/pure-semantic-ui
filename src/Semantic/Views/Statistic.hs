module Semantic.Views.Statistic (module Semantic.Views.Statistic, module Export) where

import GHC.Generics as G
import Pure.View hiding (color,horizontal)

import Semantic.Utils

data Statistic ms = Statistic_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , color :: Txt
    , floated :: Txt
    , horizontal :: Bool
    , inverted :: Bool
    , size :: Txt
    } deriving (Generic)

instance Default (Statistic ms) where
    def = (G.to gdef) { as = Div }

pattern Statistic :: Typeable ms => Statistic ms -> View ms
pattern Statistic s = View s

instance Typeable ms => Pure Statistic ms where
    render Statistic_ {..} =
        let
            cs =
                ( "ui"
                : color
                : size
                : floated # (floated <>> "floated")
                : horizontal # "horizontal"
                : inverted # "inverted"
                : "statistic"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children
