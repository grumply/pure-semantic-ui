module Semantic.Elements.Step.StepGroup where

import GHC.Generics as G
import Pure.View hiding (vertical,widths)

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Attached
import Semantic.Properties.Fluid
import Semantic.Properties.Ordered
import Semantic.Properties.Size
import Semantic.Properties.Stackable
import Semantic.Properties.Unstackable
import Semantic.Properties.Vertical
import Semantic.Properties.Widths

data StepGroup ms = StepGroup_ 
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , attached :: Maybe Txt
    , fluid :: Bool
    , ordered :: Bool
    , size :: Txt
    , stackable :: Txt
    , unstackable :: Bool
    , vertical :: Bool
    , widths :: Width
    } deriving (Generic)

instance Default (StepGroup ms) where
    def = (G.to gdef) { as = Div }

pattern StepGroup :: Typeable ms => StepGroup ms -> View ms
pattern StepGroup sg = View sg

instance Typeable ms => Pure StepGroup ms where
    render StepGroup_ {..} =
        let
            cs =
                ( "ui"
                : size
                : fluid # "fluid"
                : ordered # "ordered"
                : unstackable # "unstackable"
                : vertical # "vertical"
                : may (<>> "attached") attached
                : stackable # (stackable <>> "stackable")
                : widthProp widths def def
                : "steps"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (StepGroup ms) where
    type AsProp (StepGroup ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a sg = sg { as = a }

instance HasAttributesProp (StepGroup ms) where
    type Attribute (StepGroup ms) = Feature ms
    getAttributes = attributes
    setAttributes as sg = sg { attributes = as }

instance HasChildrenProp (StepGroup ms) where
    type Child (StepGroup ms) = View ms
    getChildren = children
    setChildren cs sg = sg { children = cs }

instance HasClassesProp (StepGroup ms) where
    getClasses = classes
    setClasses cs sg = sg { classes = cs }

instance HasAttachedProp (StepGroup ms) where
    type AttachedProp (StepGroup ms) = Maybe Txt
    getAttached = attached
    setAttached a sg = sg { attached = a }

instance HasFluidProp (StepGroup ms) where
    getFluid = fluid
    setFluid f sg = sg { fluid = f }

instance HasOrderedProp (StepGroup ms) where
    getOrdered = ordered
    setOrdered o sg = sg { ordered = o }

instance HasSizeProp (StepGroup ms) where
    getSize = size
    setSize s sg = sg { size = s }

instance HasStackableProp (StepGroup ms) where
    getStackable = stackable
    setStackable s sg = sg { stackable = s }

instance HasUnstackableProp (StepGroup ms) where
    getUnstackable = unstackable
    setUnstackable u sg = sg { unstackable = u }

instance HasVerticalProp (StepGroup ms) where
    getVertical = vertical
    setVertical v sg = sg { vertical = v }

instance HasWidthsProp (StepGroup ms) where
    getWidths = widths
    setWidths w sg = sg { widths = w }