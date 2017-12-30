module Semantic.Views.Statistic.StatisticLabel where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data StatisticLabel ms = StatisticLabel_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (StatisticLabel ms) where
    def = (G.to gdef) { as = Div }

pattern StatisticLabel :: Typeable ms => StatisticLabel ms -> View ms
pattern StatisticLabel sl = View sl

instance Typeable ms => Pure StatisticLabel ms where
    render StatisticLabel_ {..} =
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
