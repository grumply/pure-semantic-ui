module Semantic.Views.Statistic where

import GHC.Generics as G
import Pure.View hiding (color,horizontal,widths,text)

import Semantic.Utils

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  , HasColorProp(..), pattern Color
  , HasFloatedProp(..), pattern Floated
  , HasHorizontalProp(..), pattern Horizontal
  , HasInvertedProp(..), pattern Inverted
  , HasSizeProp(..), pattern Size
  , HasWidthsProp(..), pattern Widths
  , HasIsTextProp(..), pattern IsText
  )

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

pattern Statistic :: Statistic ms -> View ms
pattern Statistic s = View s

instance Pure Statistic ms where
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
                ( mergeClasses $ ClassList cs
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

data StatisticValue ms = StatisticValue_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , text :: Bool
    } deriving (Generic)

instance Default (StatisticValue ms) where
    def = (G.to gdef) { as = Div }

pattern StatisticValue :: StatisticValue ms -> View ms
pattern StatisticValue sv = View sv

instance Pure StatisticValue ms where
    render StatisticValue_ {..} =
        let
            cs =
                ( text # "text"
                : "value"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (StatisticValue ms) where
    type AsProp (StatisticValue ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a sv = sv { as = a }

instance HasAttributesProp (StatisticValue ms) where
    type Attribute (StatisticValue ms) = Feature ms
    getAttributes = attributes
    setAttributes as sv = sv { attributes = as }

instance HasChildrenProp (StatisticValue ms) where
    type Child (StatisticValue ms) = View ms
    getChildren = children
    setChildren cs sv = sv { children = cs }

instance HasClassesProp (StatisticValue ms) where
    getClasses = classes
    setClasses cs sv = sv { classes = cs }

instance HasIsTextProp (StatisticValue ms) where
    getIsText = text
    setIsText it sv = sv { text = it }
