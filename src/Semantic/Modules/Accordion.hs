module Semantic.Modules.Accordion where

import GHC.Generics as G
import Pure.View hiding (styled)

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Fluid
import Semantic.Properties.Inverted
import Semantic.Properties.Styled

import Semantic.Modules.Accordion.AccordionAccordion as Export
import Semantic.Modules.Accordion.AccordionContent as Export 
import Semantic.Modules.Accordion.AccordionTitle as Export

data Accordion ms = Accordion_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , fluid :: Bool
    , inverted :: Bool
    , styled :: Bool
    } deriving (Generic)

instance Default (Accordion ms) where
    def = (G.to gdef) { as = Div }

pattern Accordion :: Accordion ms -> View ms
pattern Accordion a = View a

instance Pure Accordion ms where
    render Accordion_ {..} =
        let
            cs =
                ( "ui"
                : "accordion"
                : fluid # "fluid"
                : inverted # "inverted"
                : styled # "styled"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (Accordion ms) where
    type AsProp (Accordion ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a acc = acc { as = a }

instance HasAttributesProp (Accordion ms) where
    type Attribute (Accordion ms) = Feature ms
    getAttributes = attributes
    setAttributes as a = a { attributes = as }

instance HasChildrenProp (Accordion ms) where
    type Child (Accordion ms) = View ms
    getChildren = children
    setChildren cs a = a { children = cs }

instance HasClassesProp (Accordion ms) where
    getClasses = classes
    setClasses cs a = a { classes = cs }

instance HasFluidProp (Accordion ms) where
    getFluid = fluid
    setFluid f a = a { fluid = f }

instance HasInvertedProp (Accordion ms) where
    getInverted = inverted
    setInverted i a = a { inverted = i }

instance HasStyledProp (Accordion ms) where
    getStyled = styled
    setStyled s a = a { styled = s }