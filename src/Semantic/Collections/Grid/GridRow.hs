module Semantic.Collections.Grid.GridRow where

import GHC.Generics as G
import Pure.View hiding (color,textAlign,verticalAlign)

import Semantic.Utils hiding (only)

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Centered
import Semantic.Properties.Color
import Semantic.Properties.Columns
import Semantic.Properties.Divided
import Semantic.Properties.Only
import Semantic.Properties.Reversed
import Semantic.Properties.Stretched
import Semantic.Properties.TextAlign
import Semantic.Properties.VerticalAlign

data GridRow ms = GridRow_
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

instance Default (GridRow ms) where
    def = (G.to gdef) { as = Div }

pattern GridRow :: Typeable ms => GridRow ms -> View ms
pattern GridRow gr = View gr

instance Typeable ms => Pure GridRow ms where
    render GridRow_ {..} =
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

instance HasAsProp (GridRow ms) where
    type AsProp (GridRow ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a gr = gr { as = a }

instance HasAttributesProp (GridRow ms) where
    type Attribute (GridRow ms) = Feature ms
    getAttributes = attributes
    setAttributes as gr = gr { attributes = as }

instance HasChildrenProp (GridRow ms) where
    type Child (GridRow ms) = View ms
    getChildren = children
    setChildren cs gr = gr { children = cs }

instance HasClassesProp (GridRow ms) where
    getClasses = classes
    setClasses cs gr = gr { classes = cs }

instance HasColorProp (GridRow ms) where
    getColor = color
    setColor c gr = gr { color = c }

instance HasColumnsProp (GridRow ms) where
    getColumns = columns
    setColumns c gr = gr { columns = c }

instance HasDividedProp (GridRow ms) where
    getDivided = divided
    setDivided d gr = gr { divided = d }

instance HasOnlyProp (GridRow ms) where
    getOnly = only
    setOnly o gr = gr { only = o }

instance HasReversedProp (GridRow ms) where
    type ReversedProp (GridRow ms) = [Txt]
    getReversed = reversed
    setReversed r gr = gr { reversed = r }

instance HasStretchedProp (GridRow ms) where
    getStretched = stretched
    setStretched s gr = gr { stretched = s }

instance HasTextAlignProp (GridRow ms) where
    getTextAlign = textAlign
    setTextAlign t gr = gr { textAlign = t }

instance HasVerticalAlignProp (GridRow ms) where
    getVerticalAlign = verticalAlign
    setVerticalAlign v gr = gr { verticalAlign = v }

