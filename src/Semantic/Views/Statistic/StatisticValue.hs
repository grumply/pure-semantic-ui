module Semantic.Views.Statistic.StatisticValue where

import GHC.Generics as G
import Pure.View hiding (text)

import Semantic.Utils

data StatisticValue ms = StatisticValue_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , text :: Bool
    } deriving (Generic)

instance Default (StatisticValue ms) where
    def = (G.to gdef) { as = Div }

pattern StatisticValue :: Typeable ms => StatisticValue ms -> View ms
pattern StatisticValue sv = View sv

instance Typeable ms => Pure StatisticValue ms where
    render StatisticValue_ {..} =
        let
            cs =
                ( text # "text"
                : "value"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children
