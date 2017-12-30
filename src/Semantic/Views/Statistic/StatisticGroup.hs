module Semantic.Views.Statistic.StatisticGroup where

import GHC.Generics as G
import Pure.View hiding (color,horizontal,widths)

import Semantic.Utils

data StatisticGroup ms = StatisticGroup_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , color :: Txt
    , horizontal :: Bool
    , inverted :: Bool
    , size :: Txt
    , widths :: Width
    } deriving (Generic)

instance Default (StatisticGroup ms) where
    def = (G.to gdef) { as = Div }

pattern StatisticGroup :: Typeable ms => StatisticGroup ms -> View ms
pattern StatisticGroup sg = View sg

instance Typeable ms => Pure StatisticGroup ms where
    render StatisticGroup_ {..} =
        let
            cs =
                ( "ui"
                : color
                : size
                : horizontal # "horizontal"
                : inverted # "inverted"
                : widthProp widths def def
                : "statistics"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children
