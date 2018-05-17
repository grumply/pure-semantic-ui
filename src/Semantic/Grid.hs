module Semantic.Grid
  ( module Properties
  , module Tools
  , Grid(..), pattern Grid
  , Column(..), pattern Column
  , Row(..), pattern Row
  ) where

import GHC.Generics as G
import Pure.Data.View
import Pure.Data.View.Patterns
import Pure.Data.Txt
import Pure.Data.HTML
import Pure.Data.Event

import Semantic.Utils hiding (only)

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Celled, Celled(..)
  , pattern Centered, Centered(..)
  , pattern Columns, Columns(..)
  , pattern IsContainer, IsContainer(..)
  , pattern Divided, Divided(..)
  , pattern Doubling, Doubling(..)
  , pattern Inverted, Inverted(..)
  , pattern Padded, Padded(..)
  , pattern Relaxed, Relaxed(..)
  , pattern Reversed, Reversed(..)
  , pattern Stackable, Stackable(..)
  , pattern Stretched, Stretched(..)
  , pattern TextAlign, TextAlign(..)
  , pattern VerticalAlign, VerticalAlign(..)
  , pattern Color, Color(..)
  , pattern Floated, Floated(..)
  , pattern OnComputer, OnComputer(..)
  , pattern OnLargeScreen, OnLargeScreen(..)
  , pattern Only, Only(..)
  , pattern OnMobile, OnMobile(..)
  , pattern OnTablet, OnTablet(..)
  , pattern OnWidescreen, OnWidescreen(..)
  , pattern Stretched, Stretched(..)
  , pattern TextAlign, TextAlign(..)
  , pattern VerticalAlign, VerticalAlign(..)
  , pattern Width, Width(..)
  , pattern One, pattern Two, pattern Three, pattern Four
  , pattern Five, pattern Six, pattern Seven, pattern Eight
  , pattern Nine, pattern Ten, pattern Eleven, pattern Twelve
  , pattern Thirteen, pattern Fourteen, pattern Fifteen, pattern Sixteen
  )

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

data Grid = Grid_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , celled :: Maybe Txt
    , centered :: Bool
    , columns :: Txt
    , container :: Bool
    , divided :: Maybe Txt
    , doubling :: Bool
    , inverted :: Bool
    , padded :: Maybe Txt
    , relaxed :: Maybe Txt
    , reversed :: [Txt]
    , stackable :: Bool
    , stretched :: Bool
    , textAlign :: Txt
    , verticalAlign :: Txt
    } deriving (Generic)

instance Default Grid where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Grid :: Grid -> Grid
pattern Grid g = g

instance Pure Grid where
    view Grid_ {..} =
        let
            cs =
                [ "ui"
                , centered # "centered"
                , container # "container"
                , doubling # "doubling"
                , inverted # "inverted"
                , stackable # "stackable"
                , stretched # "stretched"
                , may (<>> "celled") celled
                , may (<>> "divided") divided
                , may (<>> "padded") padded
                , may (<>> "relaxed") relaxed
                , multiProp reversed "reversed"
                , textAlign
                , verticalAlign
                , widthProp columns "column" True
                , "grid"
                ]
        in
            as
                : attributes
                )
                children

instance HasProp As Grid where
    type Prop As Grid = Features -> [View] -> View
    getProp _ = as
    setProp _ a g = g { as = a }

instance HasFeatures Grid where
    getFeatures = features
    setFeatures as g = g { features = as }

instance HasChildren Grid where
    getChildren = children
    setChildren cs g = g { children = cs }

instance HasProp Celled Grid where
    type Prop Celled Grid = Maybe Txt
    getProp _ = celled
    setProp _ c g = g { celled = c }

instance HasProp Centered Grid where
    type Prop Centered Grid = Bool
    getProp _ = centered
    setProp _ c g = g { centered = c }

instance HasProp Columns Grid where
    type Prop Columns Grid = Txt
    getProp _ = columns
    setProp _ c g = g { columns = c }

instance HasProp IsContainer Grid where
    type Prop IsContainer Grid = Bool
    getProp _ = container
    setProp _ ic g = g { container = ic }

instance HasProp Divided Grid where
    type Prop Divided Grid = Maybe Txt
    getProp _ = divided
    setProp _ d g = g { divided = d }

instance HasProp Doubling Grid where
    type Prop Doubling Grid = Bool
    getProp _ = doubling
    setProp _ d g = g { doubling = d }

instance HasProp Inverted Grid where
    type Prop Inverted Grid = Bool
    getProp _ = inverted
    setProp _ i g = g { inverted = i }

instance HasProp Padded Grid where
    type Prop Padded Grid = Maybe Txt
    getProp _ = padded
    setProp _ p g = g { padded = p }

instance HasProp Relaxed Grid where
    type Prop Relaxed Grid = Maybe Txt
    getProp _ = relaxed
    setProp _ r g = g { relaxed = r }

instance HasProp Reversed Grid where
    type Prop Reversed Grid = [Txt]
    getProp _ = reversed
    setProp _ r g = g { reversed = r }

instance HasProp Stackable Grid where
    type Prop Stackable Grid = Bool
    type Prop Stackable Grid = Bool
    getProp _ = stackable
    setProp _ s g = g { stackable = s }

instance HasProp Stretched Grid where
    type Prop Stretched Grid = Bool
    getProp _ = stretched
    setProp _ s g = g { stretched = s }

instance HasProp TextAlign Grid where
    type Prop TextAlign Grid = Txt
    getProp _ = textAlign
    setProp _ t g = g { textAlign = t }

instance HasProp VerticalAlign Grid where
    type Prop VerticalAlign Grid = Txt
    getProp _ = verticalAlign
    setProp _ v g = g { verticalAlign = v }

data Column = Column_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , color :: Txt
    , computer :: Txt
    , floated :: Txt
    , largeScreen :: Txt
    , mobile :: Txt
    , only :: [Txt]
    , stretched :: Bool
    , tablet :: Txt
    , textAlign :: Txt
    , verticalAlign :: Txt
    , widescreen :: Txt
    , width :: Txt
    } deriving (Generic)

instance Default Column where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Column :: Column -> Column
pattern Column gc = gc

instance Pure Column where
    view Column_ {..} =
        let
            cs =
                [ color
                , stretched # "stretched"
                , multiProp only "only"
                , textAlign
                , floated # (floated <>> "floated")
                , verticalAlign
                , widthProp computer "wide computer" def
                , widthProp largeScreen "wide large screen" def
                , widthProp mobile "wide mobile" def
                , widthProp widescreen "wide widescreen" def
                , widthProp width "wide" def
                , "column"
                ]
        in
            as
                : attributes
                )
                children

instance HasProp As Column where
    type Prop As Column = Features -> [View] -> View
    getProp _ = as
    setProp _ a gc = gc { as = a }

instance HasFeatures Column where
    getFeatures = features
    setFeatures as gc = gc { features = as }

instance HasChildren Column where
    getChildren = children
    setChildren cs gc = gc { children = cs }

instance HasProp Color Column where
    type Prop Color Column = Txt
    getProp _ = color
    setProp _ c gc = gc { color = c }

instance HasProp OnComputer Column where
    type Prop OnComputer Column = Txt
    getProp _ = computer
    setProp _ c gc = gc { computer = c }

instance HasProp Floated Column where
    type Prop Floated Column = Txt
    getProp _ = floated
    setProp _ f gc = gc { floated = f }

instance HasProp OnLargeScreen Column where
    type Prop OnLargeScreen Column = Txt
    getProp _ = largeScreen
    setProp _ ls gc = gc { largeScreen = ls }

instance HasProp OnMobile Column where
    type Prop OnMobile Column = Txt
    getProp _ = mobile
    setProp _ m gc = gc { mobile = m }

instance HasProp Only Column where
    type Prop Only Column = [Txt]
    getProp _ = only
    setProp _ o gc = gc { only = o }

instance HasProp Stretched Column where
    type Prop Stretched Column = Bool
    getProp _ = stretched
    setProp _ s gc = gc { stretched = s }

instance HasProp OnTablet Column where
    type Prop OnTablet Column = Txt
    getProp _ = tablet
    setProp _ t gc = gc { tablet = t }

instance HasProp TextAlign Column where
    type Prop TextAlign Column = Txt
    getProp _ = textAlign
    setProp _ t gc = gc { textAlign = t }

instance HasProp VerticalAlign Column where
    type Prop VerticalAlign Column = Txt
    getProp _ = verticalAlign
    setProp _ v gc = gc { verticalAlign = v }

instance HasProp OnWidescreen Column where
    type Prop OnWidescreen Column = Txt
    getProp _ = widescreen
    setProp _ w gc = gc { widescreen = w }

instance HasProp Width Column where
    type Prop Width Column = Txt
    getProp _ = width
    setProp _ w gc = gc { width = w }

data Row = Row_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , centered :: Bool
    , color :: Txt
    , columns :: Txt
    , divided :: Bool
    , only :: [Txt]
    , reversed :: [Txt]
    , stretched :: Bool
    , textAlign :: Txt
    , verticalAlign :: Txt
    } deriving (Generic)

instance Default Row where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Row :: Row -> Row
pattern Row gr = gr

instance Pure Row where
    view Row_ {..} =
        let
            cs =
                [ color
                , centered # "centered"
                , divided # "divided"
                , stretched # "stretched"
                , multiProp only "only"
                , multiProp reversed "reversed"
                , textAlign
                , verticalAlign
                , widthProp columns "columns" True
                , "row"
                ]
        in
            as
                : attributes
                )
                children

instance HasProp As Row where
    type Prop As Row = Features -> [View] -> View
    getProp _ = as
    setProp _ a gr = gr { as = a }

instance HasFeatures Row where
    getFeatures = features
    setFeatures as gr = gr { features = as }

instance HasChildren Row where
    getChildren = children
    setChildren cs gr = gr { children = cs }

instance HasProp Color Row where
    type Prop Color Row = Txt
    getProp _ = color
    setProp _ c gr = gr { color = c }

instance HasProp Columns Row where
    type Prop Columns Row = Txt
    getProp _ = columns
    setProp _ c gr = gr { columns = c }

instance HasProp Divided Row where
    type Prop Divided Row = Bool
    getProp _ = divided
    setProp _ d gr = gr { divided = d }

instance HasProp Only Row where
    type Prop Only Row = [Txt]
    getProp _ = only
    setProp _ o gr = gr { only = o }

instance HasProp Reversed Row where
    type Prop Reversed Row = [Txt]
    getProp _ = reversed
    setProp _ r gr = gr { reversed = r }

instance HasProp Stretched Row where
    type Prop Stretched Row = Bool
    getProp _ = stretched
    setProp _ s gr = gr { stretched = s }

instance HasProp TextAlign Row where
    type Prop TextAlign Row = Txt
    getProp _ = textAlign
    setProp _ t gr = gr { textAlign = t }

instance HasProp VerticalAlign Row where
    type Prop VerticalAlign Row = Txt
    getProp _ = verticalAlign
    setProp _ v gr = gr { verticalAlign = v }

