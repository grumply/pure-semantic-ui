module Semantic.Views.Statistic (module Semantic.Views.Statistic, module Export) where

import GHC.Generics as G
import Pure.View hiding (color,horizontal)

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Color
import Semantic.Properties.Floated
import Semantic.Properties.Horizontal
import Semantic.Properties.Inverted
import Semantic.Properties.Size

import Semantic.Views.Statistic.StatisticGroup as Export
import Semantic.Views.Statistic.StatisticLabel as Export
import Semantic.Views.Statistic.StatisticValue as Export

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

instance HasAsProp (Statistic ms) where
    type AsProp (Statistic ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a s = s { as = a }

instance HasAttributesProp (Statistic ms) where
    type Attribute (Statistic ms) = Feature ms
    getAttributes = attributes
    setAttributes as s = s { attributes = as }

instance HasChildrenProp (Statistic ms) where
    type Child (Statistic ms) = View ms
    getChildren = children
    setChildren cs s = s { children = cs }

instance HasClassesProp (Statistic ms) where
    getClasses = classes
    setClasses cs s = s { classes = cs }

instance HasColorProp (Statistic ms) where
    getColor = color
    setColor c s = s { color = c }

instance HasFloatedProp (Statistic ms) where
    getFloated = floated
    setFloated f s = s { floated = f }

instance HasHorizontalProp (Statistic ms) where
    getHorizontal = horizontal
    setHorizontal h s = s { horizontal = h }

instance HasInvertedProp (Statistic ms) where
    getInverted = inverted
    setInverted i s = s { inverted = i }

instance HasSizeProp (Statistic ms) where
    getSize = size
    setSize sz s = s { size = sz }