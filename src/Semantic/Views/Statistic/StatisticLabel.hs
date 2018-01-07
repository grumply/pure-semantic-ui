module Semantic.Views.Statistic.StatisticLabel where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes

data StatisticLabel ms = StatisticLabel_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (StatisticLabel ms) where
    def = (G.to gdef) { as = Div }

pattern StatisticLabel :: StatisticLabel ms -> View ms
pattern StatisticLabel sl = View sl

instance Pure StatisticLabel ms where
    render StatisticLabel_ {..} =
        let
            cs =
                ( "label"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (StatisticLabel ms) where
    type AsProp (StatisticLabel ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a sl = sl { as = a }

instance HasAttributesProp (StatisticLabel ms) where
    type Attribute (StatisticLabel ms) = Feature ms
    getAttributes = attributes
    setAttributes as sl = sl { attributes = as }

instance HasChildrenProp (StatisticLabel ms) where
    type Child (StatisticLabel ms) = View ms
    getChildren = children
    setChildren cs sl = sl { children = cs }

instance HasClassesProp (StatisticLabel ms) where
    getClasses = classes
    setClasses cs sl = sl { classes = cs }