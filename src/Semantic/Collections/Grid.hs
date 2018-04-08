module Semantic.Collections.Grid
  ( module Properties
  , module Tools
  , Grid(..), pattern Grid
  , Column(..), pattern Column
  , Row(..), pattern Row
  ) where

import GHC.Generics as G
import Pure.View hiding (name,textAlign,verticalAlign,color,width)

import Semantic.Utils hiding (only)

import Semantic.Properties as Tools ( (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  , HasCelledProp(..), pattern Celled
  , HasCenteredProp(..), pattern Centered
  , HasColumnsProp(..), pattern Columns
  , HasIsContainerProp(..), pattern IsContainer
  , HasDividedProp(..), pattern Divided
  , HasDoublingProp(..), pattern Doubling
  , HasInvertedProp(..), pattern Inverted
  , HasPaddedProp(..), pattern Padded
  , HasRelaxedProp(..), pattern Relaxed
  , HasReversedProp(..), pattern Reversed
  , HasStackableProp(..), pattern Stackable
  , HasStretchedProp(..), pattern Stretched
  , HasTextAlignProp(..), pattern TextAlign
  , HasVerticalAlignProp(..), pattern VerticalAlign
  , HasColorProp(..), pattern Color
  , HasFloatedProp(..), pattern Floated
  , HasOnComputerProp(..), pattern OnComputer
  , HasOnLargeScreenProp(..), pattern OnLargeScreen
  , HasOnlyProp(..), pattern Only
  , HasOnMobileProp(..), pattern OnMobile
  , HasOnTabletProp(..), pattern OnTablet
  , HasOnWidescreenProp(..), pattern OnWidescreen
  , HasStretchedProp(..), pattern Stretched
  , HasTextAlignProp(..), pattern TextAlign
  , HasVerticalAlignProp(..), pattern VerticalAlign
  , HasWidthProp(..), pattern Width
  )

data Grid ms = Grid_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , celled :: Maybe Txt
    , centered :: Bool
    , columns :: Width
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

instance HasAsProp (Grid ms) where
    type AsProp (Grid ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a g = g { as = a }

instance HasAttributesProp (Grid ms) where
    type Attribute (Grid ms) = Feature ms
    getAttributes = attributes
    setAttributes as g = g { attributes = as }

instance HasChildrenProp (Grid ms) where
    type Child (Grid ms) = View ms
    getChildren = children
    setChildren cs g = g { children = cs }

instance HasClassesProp (Grid ms) where
    getClasses = classes
    setClasses cs g = g { classes = cs }

instance HasCelledProp (Grid ms) where
    type CelledProp (Grid ms) = Maybe Txt
    getCelled = celled
    setCelled c g = g { celled = c }

instance HasCenteredProp (Grid ms) where
    getCentered = centered
    setCentered c g = g { centered = c }

instance HasColumnsProp (Grid ms) where
    getColumns = columns
    setColumns c g = g { columns = c }

instance HasIsContainerProp (Grid ms) where
    getIsContainer = container
    setIsContainer ic g = g { container = ic }

instance HasDividedProp (Grid ms) where
    type DividedProp (Grid ms) = Maybe Txt
    getDivided = divided
    setDivided d g = g { divided = d }

instance HasDoublingProp (Grid ms) where
    getDoubling = doubling
    setDoubling d g = g { doubling = d }

instance HasInvertedProp (Grid ms) where
    getInverted = inverted
    setInverted i g = g { inverted = i }

instance HasPaddedProp (Grid ms) where
    getPadded = padded
    setPadded p g = g { padded = p }

instance HasRelaxedProp (Grid ms) where
    getRelaxed = relaxed
    setRelaxed r g = g { relaxed = r }

instance HasReversedProp (Grid ms) where
    type ReversedProp (Grid ms) = [Txt]
    getReversed = reversed
    setReversed r g = g { reversed = r }

instance HasStackableProp (Grid ms) where
    type StackableProp (Grid ms) = Bool
    getStackable = stackable
    setStackable s g = g { stackable = s }

instance HasStretchedProp (Grid ms) where
    getStretched = stretched
    setStretched s g = g { stretched = s }

instance HasTextAlignProp (Grid ms) where
    getTextAlign = textAlign
    setTextAlign t g = g { textAlign = t }

instance HasVerticalAlignProp (Grid ms) where
    getVerticalAlign = verticalAlign
    setVerticalAlign v g = g { verticalAlign = v }

data Column ms = Column_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , color :: Txt
    , computer :: Width
    , floated :: Txt
    , largeScreen :: Width
    , mobile :: Width
    , only :: [Txt]
    , stretched :: Bool
    , tablet :: Width
    , textAlign :: Txt
    , verticalAlign :: Txt
    , widescreen :: Width
    , width :: Width
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

instance HasAsProp (Column ms) where
    type AsProp (Column ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a gc = gc { as = a }

instance HasAttributesProp (Column ms) where
    type Attribute (Column ms) = Feature ms
    getAttributes = attributes
    setAttributes as gc = gc { attributes = as }

instance HasChildrenProp (Column ms) where
    type Child (Column ms) = View ms
    getChildren = children
    setChildren cs gc = gc { children = cs }

instance HasClassesProp (Column ms) where
    getClasses = classes
    setClasses cs gc = gc { classes = cs }

instance HasColorProp (Column ms) where
    getColor = color
    setColor c gc = gc { color = c }

instance HasOnComputerProp (Column ms) where
    getOnComputer = computer
    setOnComputer c gc = gc { computer = c }

instance HasFloatedProp (Column ms) where
    getFloated = floated
    setFloated f gc = gc { floated = f }

instance HasOnLargeScreenProp (Column ms) where
    getOnLargeScreen = largeScreen
    setOnLargeScreen ls gc = gc { largeScreen = ls }

instance HasOnMobileProp (Column ms) where
    getOnMobile = mobile
    setOnMobile m gc = gc { mobile = m }

instance HasOnlyProp (Column ms) where
    getOnly = only
    setOnly o gc = gc { only = o }

instance HasStretchedProp (Column ms) where
    getStretched = stretched
    setStretched s gc = gc { stretched = s }

instance HasOnTabletProp (Column ms) where
    getOnTablet = tablet
    setOnTablet t gc = gc { tablet = t }

instance HasTextAlignProp (Column ms) where
    getTextAlign = textAlign
    setTextAlign t gc = gc { textAlign = t }

instance HasVerticalAlignProp (Column ms) where
    getVerticalAlign = verticalAlign
    setVerticalAlign v gc = gc { verticalAlign = v }

instance HasOnWidescreenProp (Column ms) where
    getOnWidescreen = widescreen
    setOnWidescreen w gc = gc { widescreen = w }

instance HasWidthProp (Column ms) where
    getWidth = width
    setWidth w gc = gc { width = w }
data Row ms = Row_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , centered :: Bool
    , color :: Txt
    , columns :: Width
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

instance HasAsProp (Row ms) where
    type AsProp (Row ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a gr = gr { as = a }

instance HasAttributesProp (Row ms) where
    type Attribute (Row ms) = Feature ms
    getAttributes = attributes
    setAttributes as gr = gr { attributes = as }

instance HasChildrenProp (Row ms) where
    type Child (Row ms) = View ms
    getChildren = children
    setChildren cs gr = gr { children = cs }

instance HasClassesProp (Row ms) where
    getClasses = classes
    setClasses cs gr = gr { classes = cs }

instance HasColorProp (Row ms) where
    getColor = color
    setColor c gr = gr { color = c }

instance HasColumnsProp (Row ms) where
    getColumns = columns
    setColumns c gr = gr { columns = c }

instance HasDividedProp (Row ms) where
    getDivided = divided
    setDivided d gr = gr { divided = d }

instance HasOnlyProp (Row ms) where
    getOnly = only
    setOnly o gr = gr { only = o }

instance HasReversedProp (Row ms) where
    type ReversedProp (Row ms) = [Txt]
    getReversed = reversed
    setReversed r gr = gr { reversed = r }

instance HasStretchedProp (Row ms) where
    getStretched = stretched
    setStretched s gr = gr { stretched = s }

instance HasTextAlignProp (Row ms) where
    getTextAlign = textAlign
    setTextAlign t gr = gr { textAlign = t }

instance HasVerticalAlignProp (Row ms) where
    getVerticalAlign = verticalAlign
    setVerticalAlign v gr = gr { verticalAlign = v }

