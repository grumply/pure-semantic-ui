module Semantic.Views.Statistic
  ( module Properties
  , module Tools
  , Statistic(..), pattern Statistic
  , Group(..), pattern Group
  , Label(..), pattern Label
  , Value(..), pattern Value
  ) where

import GHC.Generics as G
import Pure.View hiding (color,horizontal,widths,text,Value,Label)

import Semantic.Utils

import Semantic.Properties as Tools ( (<|), (<||>), (|>) )

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

data Group ms = Group_
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

instance Default (Group ms) where
    def = (G.to gdef) { as = Div }

pattern Group :: Group ms -> View ms
pattern Group sg = View sg

instance Pure Group ms where
    render Group_ {..} =
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

instance HasAsProp (Group ms) where
    type AsProp (Group ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a sg = sg { as = a }

instance HasAttributesProp (Group ms) where
    type Attribute (Group ms) = Feature ms
    getAttributes = attributes
    setAttributes as sg = sg { attributes = as }

instance HasChildrenProp (Group ms) where
    type Child (Group ms) = View ms
    getChildren = children
    setChildren cs sg = sg { children = cs }

instance HasClassesProp (Group ms) where
    getClasses = classes
    setClasses cs sg = sg { classes = cs }

instance HasColorProp (Group ms) where
    getColor = color
    setColor c s = s { color = c }

instance HasHorizontalProp (Group ms) where
    getHorizontal = horizontal
    setHorizontal h s = s { horizontal = h }

instance HasInvertedProp (Group ms) where
    getInverted = inverted
    setInverted i s = s { inverted = i }

instance HasSizeProp (Group ms) where
    getSize = size
    setSize sz s = s { size = sz }

instance HasWidthsProp (Group ms) where
    getWidths = widths
    setWidths w s = s { widths = w }

data Label ms = Label_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Label ms) where
    def = (G.to gdef) { as = Div }

pattern Label :: Label ms -> View ms
pattern Label sl = View sl

instance Pure Label ms where
    render Label_ {..} =
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

instance HasAsProp (Label ms) where
    type AsProp (Label ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a sl = sl { as = a }

instance HasAttributesProp (Label ms) where
    type Attribute (Label ms) = Feature ms
    getAttributes = attributes
    setAttributes as sl = sl { attributes = as }

instance HasChildrenProp (Label ms) where
    type Child (Label ms) = View ms
    getChildren = children
    setChildren cs sl = sl { children = cs }

instance HasClassesProp (Label ms) where
    getClasses = classes
    setClasses cs sl = sl { classes = cs }

data Value ms = Value_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , text :: Bool
    } deriving (Generic)

instance Default (Value ms) where
    def = (G.to gdef) { as = Div }

pattern Value :: Value ms -> View ms
pattern Value sv = View sv

instance Pure Value ms where
    render Value_ {..} =
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

instance HasAsProp (Value ms) where
    type AsProp (Value ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a sv = sv { as = a }

instance HasAttributesProp (Value ms) where
    type Attribute (Value ms) = Feature ms
    getAttributes = attributes
    setAttributes as sv = sv { attributes = as }

instance HasChildrenProp (Value ms) where
    type Child (Value ms) = View ms
    getChildren = children
    setChildren cs sv = sv { children = cs }

instance HasClassesProp (Value ms) where
    getClasses = classes
    setClasses cs sv = sv { classes = cs }

instance HasIsTextProp (Value ms) where
    getIsText = text
    setIsText it sv = sv { text = it }
