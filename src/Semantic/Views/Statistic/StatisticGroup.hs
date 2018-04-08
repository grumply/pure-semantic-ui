module Semantic.Views.Statistic.StatisticGroup where

import GHC.Generics as G
import Pure.View hiding (color,horizontal,widths)

import Semantic.Utils

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  , HasColorProp(..), pattern Color
  , HasHorizontalProp(..), pattern Horizontal
  , HasInvertedProp(..), pattern Inverted
  , HasSizeProp(..), pattern Size
  , HasWidthsProp(..), pattern Widths
  )

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

pattern StatisticGroup :: StatisticGroup ms -> View ms
pattern StatisticGroup sg = View sg

instance Pure StatisticGroup ms where
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
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (StatisticGroup ms) where
    type AsProp (StatisticGroup ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a sg = sg { as = a }

instance HasAttributesProp (StatisticGroup ms) where
    type Attribute (StatisticGroup ms) = Feature ms
    getAttributes = attributes
    setAttributes as sg = sg { attributes = as }

instance HasChildrenProp (StatisticGroup ms) where
    type Child (StatisticGroup ms) = View ms
    getChildren = children
    setChildren cs sg = sg { children = cs }

instance HasClassesProp (StatisticGroup ms) where
    getClasses = classes
    setClasses cs sg = sg { classes = cs }

instance HasColorProp (StatisticGroup ms) where
    getColor = color
    setColor c s = s { color = c }

instance HasHorizontalProp (StatisticGroup ms) where
    getHorizontal = horizontal
    setHorizontal h s = s { horizontal = h }

instance HasInvertedProp (StatisticGroup ms) where
    getInverted = inverted
    setInverted i s = s { inverted = i }

instance HasSizeProp (StatisticGroup ms) where
    getSize = size
    setSize sz s = s { size = sz }

instance HasWidthsProp (StatisticGroup ms) where
    getWidths = widths
    setWidths w s = s { widths = w }
