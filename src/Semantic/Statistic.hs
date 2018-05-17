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

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Color, Color(..)
  , pattern Floated, Floated(..)
  , pattern Horizontal, Horizontal(..)
  , pattern Inverted, Inverted(..)
  , pattern Size, Size(..)
  , pattern Widths, Widths(..)
  , pattern IsText, IsText(..)
  , pattern One, pattern Two, pattern Three, pattern Four
  , pattern Five, pattern Six, pattern Seven, pattern Eight
  , pattern Nine, pattern Ten, pattern Eleven, pattern Twelve
  , pattern Thirteen, pattern Fourteen, pattern Fifteen, pattern Sixteen
  )

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

data Statistic = Statistic_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , color :: Txt
    , floated :: Txt
    , horizontal :: Bool
    , inverted :: Bool
    , size :: Txt
    } deriving (Generic)

instance Default Statistic where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Statistic :: Statistic -> Statistic
pattern Statistic s = s

instance Pure Statistic where
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
                )
        in
            as
                : attributes
                )
                children

instance HasProp As Statistic where
    type Prop As Statistic = Features -> [View] -> View
    getProp _ = as
    setProp _ a s = s { as = a }

instance HasFeatures Statistic where
    getFeatures = features
    setFeatures as s = s { features = as }

instance HasChildren Statistic where
    getChildren = children
    setChildren cs s = s { children = cs }


instance HasProp Color Statistic where
    type Prop Color Statistic = Txt
    getProp _ = color
    setProp _ c s = s { color = c }

instance HasProp Floated Statistic where
    type Prop Floated Statistic = Txt
    getProp _ = floated
    setProp _ f s = s { floated = f }

instance HasProp Horizontal Statistic where
    type Prop Horizontal Statistic = Bool
    getProp _ = horizontal
    setProp _ h s = s { horizontal = h }

instance HasProp Inverted Statistic where
    type Prop Inverted Statistic = Bool
    getProp _ = inverted
    setProp _ i s = s { inverted = i }

instance HasProp Size Statistic where
    type Prop Size Statistic = Txt
    getProp _ = size
    setProp _ sz s = s { size = sz }

data Group = Group_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , color :: Txt
    , horizontal :: Bool
    , inverted :: Bool
    , size :: Txt
    , widths :: Txt
    } deriving (Generic)

instance Default Group where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Group :: Group -> Group
pattern Group sg = sg

instance Pure Group where
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
                )
        in
            as
                : attributes
                )
                children

instance HasProp As Group where
    type Prop As Group = Features -> [View] -> View
    getProp _ = as
    setProp _ a sg = sg { as = a }

instance HasFeatures Group where
    getFeatures = features
    setFeatures as sg = sg { features = as }

instance HasChildren Group where
    getChildren = children
    setChildren cs sg = sg { children = cs }


instance HasProp Color Group where
    type Prop Color Group = Txt
    getProp _ = color
    setProp _ c s = s { color = c }

instance HasProp Horizontal Group where
    type Prop Horizontal Group = Bool
    getProp _ = horizontal
    setProp _ h s = s { horizontal = h }

instance HasProp Inverted Group where
    type Prop Inverted Group = Bool
    getProp _ = inverted
    setProp _ i s = s { inverted = i }

instance HasProp Size Group where
    type Prop Size Group = Txt
    getProp _ = size
    setProp _ sz s = s { size = sz }

instance HasProp Widths Group where
    type Prop Widths Group = Txt
    getProp _ = widths
    setProp _ w s = s { widths = w }

data Label = Label_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Label where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Label :: Label -> Label
pattern Label sl = sl

instance Pure Label where
    render Label_ {..} =
        let
            cs =
                ( "label"
                )
        in
            as
                : attributes
                )
                children

instance HasProp As Label where
    type Prop As Label = Features -> [View] -> View
    getProp _ = as
    setProp _ a sl = sl { as = a }

instance HasFeatures Label where
    getFeatures = features
    setFeatures as sl = sl { features = as }

instance HasChildren Label where
    getChildren = children
    setChildren cs sl = sl { children = cs }


data Value = Value_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , text :: Bool
    } deriving (Generic)

instance Default Value where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Value :: Value -> Value
pattern Value sv = sv

instance Pure Value where
    render Value_ {..} =
        let
            cs =
                ( text # "text"
                : "value"
                )
        in
            as
                : attributes
                )
                children

instance HasProp As Value where
    type Prop As Value = Features -> [View] -> View
    getProp _ = as
    setProp _ a sv = sv { as = a }

instance HasFeatures Value where
    getFeatures = features
    setFeatures as sv = sv { features = as }

instance HasChildren Value where
    getChildren = children
    setChildren cs sv = sv { children = cs }


instance HasProp IsText Value where
    type Prop IsText Value = Bool
    getProp _ = text
    setProp _ it sv = sv { text = it }
