module Semantic.Elements.Button.ButtonGroup where

import GHC.Generics as G
import Pure.View hiding (color,vertical,widths,Button,Label)
import qualified Pure.View as HTML

import Semantic.Utils

import Semantic.Elements.Icon

import Semantic.Properties.As
import Semantic.Properties.Attached
import Semantic.Properties.Attributes
import Semantic.Properties.Basic
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Color
import Semantic.Properties.Compact
import Semantic.Properties.Floated
import Semantic.Properties.Fluid
import Semantic.Properties.Inverted
import Semantic.Properties.Labeled
import Semantic.Properties.Negative
import Semantic.Properties.Positive
import Semantic.Properties.Primary
import Semantic.Properties.Secondary
import Semantic.Properties.Size
import Semantic.Properties.Toggle
import Semantic.Properties.Vertical
import Semantic.Properties.Widths

data ButtonGroup ms = ButtonGroup_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attached :: Maybe Txt
    , attributes :: [Feature ms]
    , basic :: Bool
    , children :: [View ms]
    , classes :: [Txt]
    , color :: Txt
    , compact :: Bool
    , floated :: Txt
    , fluid :: Bool
    , inverted :: Bool
    , labeled :: Bool
    , negative :: Bool
    , positive :: Bool
    , primary :: Bool
    , secondary :: Bool
    , size :: Txt
    , toggle :: Bool
    , vertical :: Bool
    , widths :: Width
    } deriving (Generic)

instance Default (ButtonGroup ms) where
    def = (G.to gdef) { as = Div }

pattern ButtonGroup :: ButtonGroup ms -> View ms
pattern ButtonGroup bc = View bc

instance Pure ButtonGroup ms where
    render ButtonGroup_ {..} =
        let
            icon =
                foldPures (\(Icon_ {}) -> const True) False children
            
            cs =
                ( "ui"
                : color
                : size
                : basic # "basic"
                : compact # "compact"
                : fluid # "fluid"
                : icon # "icon"
                : inverted # "inverted"
                : labeled # "labeled"
                : negative # "negative"
                : positive # "positive"
                : primary # "primary"
                : secondary # "secondary"
                : toggle # "toggle"
                : vertical # "vertical"
                : useKeyOrValueAndKey attached "attached"
                : floated # ("floated" <<>> floated)
                : widthProp widths def def
                : "buttons"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (ButtonGroup ms) where
    type AsProp (ButtonGroup ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f bg = bg { as = f }

instance HasAttachedProp (ButtonGroup ms) where
    type AttachedProp (ButtonGroup ms) = Maybe Txt
    getAttached = attached
    setAttached attach bg = bg { attached = attach }

instance HasAttributesProp (ButtonGroup ms) where
    type Attribute (ButtonGroup ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs bg = bg { attributes = cs }

instance HasBasicProp (ButtonGroup ms) where
    getBasic = basic
    setBasic b bg = bg { basic = b }

instance HasChildrenProp (ButtonGroup ms) where
    type Child (ButtonGroup ms) = View ms
    getChildren = children
    setChildren cs bg = bg { children = cs }

instance HasClassesProp (ButtonGroup ms) where
    getClasses = classes
    setClasses cs bg = bg { classes = cs }

instance HasColorProp (ButtonGroup ms) where
    getColor = color
    setColor c bg = bg { color = c }

instance HasCompactProp (ButtonGroup ms) where
    getCompact = compact
    setCompact c bg = bg { compact = c }

instance HasFloatedProp (ButtonGroup ms) where
    getFloated = floated
    setFloated f bg = bg { floated = f }

instance HasFluidProp (ButtonGroup ms) where
    getFluid = fluid
    setFluid f bg = bg { fluid = f }

instance HasInvertedProp (ButtonGroup ms) where
    getInverted = inverted
    setInverted i bg = bg { inverted = i }

instance HasLabeledProp (ButtonGroup ms) where
    getLabeled = labeled
    setLabeled l bg = bg { labeled = l }

instance HasNegativeProp (ButtonGroup ms) where
    getNegative = negative
    setNegative n bg = bg { negative = n }

instance HasPositiveProp (ButtonGroup ms) where
    getPositive = positive
    setPositive p bg = bg { positive = p }

instance HasPrimaryProp (ButtonGroup ms) where
    getPrimary = primary
    setPrimary p bg = bg { primary = p }

instance HasSecondaryProp (ButtonGroup ms) where
    getSecondary = secondary
    setSecondary s bg = bg { secondary = s }

instance HasSizeProp (ButtonGroup ms) where
    getSize = size
    setSize s bg = bg { size = s }

instance HasToggleProp (ButtonGroup ms) where
    getToggle = toggle
    setToggle t bg = bg { toggle = t }

instance HasVerticalProp (ButtonGroup ms) where
    getVertical = vertical
    setVertical v bg = bg { vertical = v }

instance HasWidthsProp (ButtonGroup ms) where
    getWidths = widths
    setWidths w bg = bg { widths = w }