module Semantic.Collections.Grid (module Semantic.Collections.Grid, module Export) where

import GHC.Generics as G
import Pure.View hiding (name,textAlign,verticalAlign)

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Celled
import Semantic.Properties.Centered
import Semantic.Properties.Columns
import Semantic.Properties.IsContainer
import Semantic.Properties.Divided
import Semantic.Properties.Doubling
import Semantic.Properties.Inverted
import Semantic.Properties.Padded
import Semantic.Properties.Relaxed
import Semantic.Properties.Reversed
import Semantic.Properties.Stackable
import Semantic.Properties.Stretched
import Semantic.Properties.TextAlign
import Semantic.Properties.VerticalAlign

import Semantic.Collections.Grid.GridColumn as Export
import Semantic.Collections.Grid.GridRow as Export

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