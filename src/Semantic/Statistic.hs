module Semantic.Statistic
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

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>), (!), (%) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Classes, Classes(..)
  , pattern Color, Color(..)
  , pattern Floated, Floated(..)
  , pattern Horizontal, Horizontal(..)
  , pattern Inverted, Inverted(..)
  , pattern Size, Size(..)
  , pattern Widths, Widths(..)
  , pattern IsText, IsText(..)
  )

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

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

instance HasProp As (Statistic ms) where
    type Prop As (Statistic ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a s = s { as = a }

instance HasProp Attributes (Statistic ms) where
    type Prop Attributes (Statistic ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as s = s { attributes = as }

instance HasProp Children (Statistic ms) where
    type Prop Children (Statistic ms) = [View ms]
    getProp _ = children
    setProp _ cs s = s { children = cs }

instance HasProp Classes (Statistic ms) where
    type Prop Classes (Statistic ms) = [Txt]
    getProp _ = classes
    setProp _ cs s = s { classes = cs }

instance HasProp Color (Statistic ms) where
    type Prop Color (Statistic ms) = Txt
    getProp _ = color
    setProp _ c s = s { color = c }

instance HasProp Floated (Statistic ms) where
    type Prop Floated (Statistic ms) = Txt
    getProp _ = floated
    setProp _ f s = s { floated = f }

instance HasProp Horizontal (Statistic ms) where
    type Prop Horizontal (Statistic ms) = Bool
    getProp _ = horizontal
    setProp _ h s = s { horizontal = h }

instance HasProp Inverted (Statistic ms) where
    type Prop Inverted (Statistic ms) = Bool
    getProp _ = inverted
    setProp _ i s = s { inverted = i }

instance HasProp Size (Statistic ms) where
    type Prop Size (Statistic ms) = Txt
    getProp _ = size
    setProp _ sz s = s { size = sz }

data Group ms = Group_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , color :: Txt
    , horizontal :: Bool
    , inverted :: Bool
    , size :: Txt
    , widths :: Txt
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

instance HasProp As (Group ms) where
    type Prop As (Group ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a sg = sg { as = a }

instance HasProp Attributes (Group ms) where
    type Prop Attributes (Group ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as sg = sg { attributes = as }

instance HasProp Children (Group ms) where
    type Prop Children (Group ms) = [View ms]
    getProp _ = children
    setProp _ cs sg = sg { children = cs }

instance HasProp Classes (Group ms) where
    type Prop Classes (Group ms) = [Txt]
    getProp _ = classes
    setProp _ cs sg = sg { classes = cs }

instance HasProp Color (Group ms) where
    type Prop Color (Group ms) = Txt
    getProp _ = color
    setProp _ c s = s { color = c }

instance HasProp Horizontal (Group ms) where
    type Prop Horizontal (Group ms) = Bool
    getProp _ = horizontal
    setProp _ h s = s { horizontal = h }

instance HasProp Inverted (Group ms) where
    type Prop Inverted (Group ms) = Bool
    getProp _ = inverted
    setProp _ i s = s { inverted = i }

instance HasProp Size (Group ms) where
    type Prop Size (Group ms) = Txt
    getProp _ = size
    setProp _ sz s = s { size = sz }

instance HasProp Widths (Group ms) where
    type Prop Widths (Group ms) = Txt
    getProp _ = widths
    setProp _ w s = s { widths = w }

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

instance HasProp As (Label ms) where
    type Prop As (Label ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a sl = sl { as = a }

instance HasProp Attributes (Label ms) where
    type Prop Attributes (Label ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as sl = sl { attributes = as }

instance HasProp Children (Label ms) where
    type Prop Children (Label ms) = [View ms]
    getProp _ = children
    setProp _ cs sl = sl { children = cs }

instance HasProp Classes (Label ms) where
    type Prop Classes (Label ms) = [Txt]
    getProp _ = classes
    setProp _ cs sl = sl { classes = cs }

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

instance HasProp As (Value ms) where
    type Prop As (Value ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a sv = sv { as = a }

instance HasProp Attributes (Value ms) where
    type Prop Attributes (Value ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as sv = sv { attributes = as }

instance HasProp Children (Value ms) where
    type Prop Children (Value ms) = [View ms]
    getProp _ = children
    setProp _ cs sv = sv { children = cs }

instance HasProp Classes (Value ms) where
    type Prop Classes (Value ms) = [Txt]
    getProp _ = classes
    setProp _ cs sv = sv { classes = cs }

instance HasProp IsText (Value ms) where
    type Prop IsText (Value ms) = Bool
    getProp _ = text
    setProp _ it sv = sv { text = it }
