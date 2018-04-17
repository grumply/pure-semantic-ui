module Semantic.Grid
  ( module Properties
  , module Tools
  , Grid(..), pattern Grid
  , Column(..), pattern Column
  , Row(..), pattern Row
  ) where

import GHC.Generics as G
import Pure.View hiding (name,textAlign,verticalAlign,color,width)

import Semantic.Utils hiding (only)

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>), (!) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Classes, Classes(..)
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
  )

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

data Grid ms = Grid_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
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

instance Default (Grid ms) where
    def = (G.to gdef) { as = Div }

pattern Grid :: Grid ms -> View ms
pattern Grid g = View g

instance Pure Grid ms where
    render Grid_ {..} =
        let
            cs =
                ( "ui"
                : centered # "centered"
                : container # "container"
                : doubling # "doubling"
                : inverted # "inverted"
                : stackable # "stackable"
                : stretched # "stretched"
                : may (<>> "celled") celled
                : may (<>> "divided") divided
                : may (<>> "padded") padded
                : may (<>> "relaxed") relaxed
                : multiProp reversed "reversed"
                : textAlign
                : verticalAlign
                : widthProp columns "column" True
                : "grid"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (Grid ms) where
    type Prop As (Grid ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a g = g { as = a }

instance HasProp Attributes (Grid ms) where
    type Prop Attributes (Grid ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as g = g { attributes = as }

instance HasProp Children (Grid ms) where
    type Prop Children (Grid ms) = [View ms]
    getProp _ = children
    setProp _ cs g = g { children = cs }

instance HasProp Classes (Grid ms) where
    type Prop Classes (Grid ms) = [Txt]
    getProp _ = classes
    setProp _ cs g = g { classes = cs }

instance HasProp Celled (Grid ms) where
    type Prop Celled (Grid ms) = Maybe Txt
    getProp _ = celled
    setProp _ c g = g { celled = c }

instance HasProp Centered (Grid ms) where
    type Prop Centered (Grid ms) = Bool
    getProp _ = centered
    setProp _ c g = g { centered = c }

instance HasProp Columns (Grid ms) where
    type Prop Columns (Grid ms) = Txt
    getProp _ = columns
    setProp _ c g = g { columns = c }

instance HasProp IsContainer (Grid ms) where
    type Prop IsContainer (Grid ms) = Bool
    getProp _ = container
    setProp _ ic g = g { container = ic }

instance HasProp Divided (Grid ms) where
    type Prop Divided (Grid ms) = Maybe Txt
    getProp _ = divided
    setProp _ d g = g { divided = d }

instance HasProp Doubling (Grid ms) where
    type Prop Doubling (Grid ms) = Bool
    getProp _ = doubling
    setProp _ d g = g { doubling = d }

instance HasProp Inverted (Grid ms) where
    type Prop Inverted (Grid ms) = Bool
    getProp _ = inverted
    setProp _ i g = g { inverted = i }

instance HasProp Padded (Grid ms) where
    type Prop Padded (Grid ms) = Maybe Txt
    getProp _ = padded
    setProp _ p g = g { padded = p }

instance HasProp Relaxed (Grid ms) where
    type Prop Relaxed (Grid ms) = Maybe Txt
    getProp _ = relaxed
    setProp _ r g = g { relaxed = r }

instance HasProp Reversed (Grid ms) where
    type Prop Reversed (Grid ms) = [Txt]
    getProp _ = reversed
    setProp _ r g = g { reversed = r }

instance HasProp Stackable (Grid ms) where
    type Prop Stackable (Grid ms) = Bool
    type Prop Stackable (Grid ms) = Bool
    getProp _ = stackable
    setProp _ s g = g { stackable = s }

instance HasProp Stretched (Grid ms) where
    type Prop Stretched (Grid ms) = Bool
    getProp _ = stretched
    setProp _ s g = g { stretched = s }

instance HasProp TextAlign (Grid ms) where
    type Prop TextAlign (Grid ms) = Txt
    getProp _ = textAlign
    setProp _ t g = g { textAlign = t }

instance HasProp VerticalAlign (Grid ms) where
    type Prop VerticalAlign (Grid ms) = Txt
    getProp _ = verticalAlign
    setProp _ v g = g { verticalAlign = v }

data Column ms = Column_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
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

instance Default (Column ms) where
    def = (G.to gdef) { as = Div }

pattern Column :: Column ms -> View ms
pattern Column gc = View gc

instance Pure Column ms where
    render Column_ {..} =
        let
            cs =
                ( color
                : stretched # "stretched"
                : multiProp only "only"
                : textAlign
                : floated # (floated <>> "floated")
                : verticalAlign
                : widthProp computer "wide computer" def
                : widthProp largeScreen "wide large screen" def
                : widthProp mobile "wide mobile" def
                : widthProp widescreen "wide widescreen" def
                : widthProp width "wide" def
                : "column"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (Column ms) where
    type Prop As (Column ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a gc = gc { as = a }

instance HasProp Attributes (Column ms) where
    type Prop Attributes (Column ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as gc = gc { attributes = as }

instance HasProp Children (Column ms) where
    type Prop Children (Column ms) = [View ms]
    getProp _ = children
    setProp _ cs gc = gc { children = cs }

instance HasProp Classes (Column ms) where
    type Prop Classes (Column ms) = [Txt]
    getProp _ = classes
    setProp _ cs gc = gc { classes = cs }

instance HasProp Color (Column ms) where
    type Prop Color (Column ms) = Txt
    getProp _ = color
    setProp _ c gc = gc { color = c }

instance HasProp OnComputer (Column ms) where
    type Prop OnComputer (Column ms) = Txt
    getProp _ = computer
    setProp _ c gc = gc { computer = c }

instance HasProp Floated (Column ms) where
    type Prop Floated (Column ms) = Txt
    getProp _ = floated
    setProp _ f gc = gc { floated = f }

instance HasProp OnLargeScreen (Column ms) where
    type Prop OnLargeScreen (Column ms) = Txt
    getProp _ = largeScreen
    setProp _ ls gc = gc { largeScreen = ls }

instance HasProp OnMobile (Column ms) where
    type Prop OnMobile (Column ms) = Txt
    getProp _ = mobile
    setProp _ m gc = gc { mobile = m }

instance HasProp Only (Column ms) where
    type Prop Only (Column ms) = [Txt]
    getProp _ = only
    setProp _ o gc = gc { only = o }

instance HasProp Stretched (Column ms) where
    type Prop Stretched (Column ms) = Bool
    getProp _ = stretched
    setProp _ s gc = gc { stretched = s }

instance HasProp OnTablet (Column ms) where
    type Prop OnTablet (Column ms) = Txt
    getProp _ = tablet
    setProp _ t gc = gc { tablet = t }

instance HasProp TextAlign (Column ms) where
    type Prop TextAlign (Column ms) = Txt
    getProp _ = textAlign
    setProp _ t gc = gc { textAlign = t }

instance HasProp VerticalAlign (Column ms) where
    type Prop VerticalAlign (Column ms) = Txt
    getProp _ = verticalAlign
    setProp _ v gc = gc { verticalAlign = v }

instance HasProp OnWidescreen (Column ms) where
    type Prop OnWidescreen (Column ms) = Txt
    getProp _ = widescreen
    setProp _ w gc = gc { widescreen = w }

instance HasProp Width (Column ms) where
    type Prop Width (Column ms) = Txt
    getProp _ = width
    setProp _ w gc = gc { width = w }

data Row ms = Row_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
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

instance Default (Row ms) where
    def = (G.to gdef) { as = Div }

pattern Row :: Row ms -> View ms
pattern Row gr = View gr

instance Pure Row ms where
    render Row_ {..} =
        let
            cs =
                ( color
                : centered # "centered"
                : divided # "divided"
                : stretched # "stretched"
                : multiProp only "only"
                : multiProp reversed "reversed"
                : textAlign
                : verticalAlign
                : widthProp columns "columns" True
                : "row"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (Row ms) where
    type Prop As (Row ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a gr = gr { as = a }

instance HasProp Attributes (Row ms) where
    type Prop Attributes (Row ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as gr = gr { attributes = as }

instance HasProp Children (Row ms) where
    type Prop Children (Row ms) = [View ms]
    getProp _ = children
    setProp _ cs gr = gr { children = cs }

instance HasProp Classes (Row ms) where
    type Prop Classes (Row ms) = [Txt]
    getProp _ = classes
    setProp _ cs gr = gr { classes = cs }

instance HasProp Color (Row ms) where
    type Prop Color (Row ms) = Txt
    getProp _ = color
    setProp _ c gr = gr { color = c }

instance HasProp Columns (Row ms) where
    type Prop Columns (Row ms) = Txt
    getProp _ = columns
    setProp _ c gr = gr { columns = c }

instance HasProp Divided (Row ms) where
    type Prop Divided (Row ms) = Bool
    getProp _ = divided
    setProp _ d gr = gr { divided = d }

instance HasProp Only (Row ms) where
    type Prop Only (Row ms) = [Txt]
    getProp _ = only
    setProp _ o gr = gr { only = o }

instance HasProp Reversed (Row ms) where
    type Prop Reversed (Row ms) = [Txt]
    getProp _ = reversed
    setProp _ r gr = gr { reversed = r }

instance HasProp Stretched (Row ms) where
    type Prop Stretched (Row ms) = Bool
    getProp _ = stretched
    setProp _ s gr = gr { stretched = s }

instance HasProp TextAlign (Row ms) where
    type Prop TextAlign (Row ms) = Txt
    getProp _ = textAlign
    setProp _ t gr = gr { textAlign = t }

instance HasProp VerticalAlign (Row ms) where
    type Prop VerticalAlign (Row ms) = Txt
    getProp _ = verticalAlign
    setProp _ v gr = gr { verticalAlign = v }

