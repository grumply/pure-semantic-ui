module Semantic.Collections.Grid.GridColumn where

import GHC.Generics as G
import Pure.View hiding (color,textAlign,verticalAlign,width)

import Semantic.Utils hiding (only)

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Color
import Semantic.Properties.Floated
import Semantic.Properties.OnComputer
import Semantic.Properties.OnLargeScreen
import Semantic.Properties.Only
import Semantic.Properties.OnMobile
import Semantic.Properties.OnTablet
import Semantic.Properties.OnWidescreen
import Semantic.Properties.Stretched
import Semantic.Properties.TextAlign
import Semantic.Properties.VerticalAlign
import Semantic.Properties.Width

data GridColumn ms = GridColumn_
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

instance Default (GridColumn ms) where
    def = (G.to gdef) { as = Div }

pattern GridColumn :: GridColumn ms -> View ms
pattern GridColumn gc = View gc

instance Pure GridColumn ms where
    render GridColumn_ {..} =
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

instance HasAsProp (GridColumn ms) where
    type AsProp (GridColumn ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a gc = gc { as = a }

instance HasAttributesProp (GridColumn ms) where
    type Attribute (GridColumn ms) = Feature ms
    getAttributes = attributes
    setAttributes as gc = gc { attributes = as }

instance HasChildrenProp (GridColumn ms) where
    type Child (GridColumn ms) = View ms
    getChildren = children
    setChildren cs gc = gc { children = cs }

instance HasClassesProp (GridColumn ms) where
    getClasses = classes
    setClasses cs gc = gc { classes = cs }

instance HasColorProp (GridColumn ms) where
    getColor = color
    setColor c gc = gc { color = c }

instance HasOnComputerProp (GridColumn ms) where
    getOnComputer = computer
    setOnComputer c gc = gc { computer = c }

instance HasFloatedProp (GridColumn ms) where
    getFloated = floated
    setFloated f gc = gc { floated = f }

instance HasOnLargeScreenProp (GridColumn ms) where
    getOnLargeScreen = largeScreen
    setOnLargeScreen ls gc = gc { largeScreen = ls }

instance HasOnMobileProp (GridColumn ms) where
    getOnMobile = mobile
    setOnMobile m gc = gc { mobile = m }

instance HasOnlyProp (GridColumn ms) where
    getOnly = only
    setOnly o gc = gc { only = o }

instance HasStretchedProp (GridColumn ms) where
    getStretched = stretched
    setStretched s gc = gc { stretched = s }

instance HasOnTabletProp (GridColumn ms) where
    getOnTablet = tablet
    setOnTablet t gc = gc { tablet = t }

instance HasTextAlignProp (GridColumn ms) where
    getTextAlign = textAlign
    setTextAlign t gc = gc { textAlign = t }

instance HasVerticalAlignProp (GridColumn ms) where
    getVerticalAlign = verticalAlign
    setVerticalAlign v gc = gc { verticalAlign = v }

instance HasOnWidescreenProp (GridColumn ms) where
    getOnWidescreen = widescreen
    setOnWidescreen w gc = gc { widescreen = w }

instance HasWidthProp (GridColumn ms) where
    getWidth = width
    setWidth w gc = gc { width = w }