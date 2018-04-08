module Semantic.Modules.Accordion.AccordionAccordion where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  )

data AccordionAccordion ms = AccordionAccordion_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (AccordionAccordion ms) where
    def = (G.to gdef) { as = Div }

pattern AccordionAccordion :: AccordionAccordion ms -> View ms
pattern AccordionAccordion a = View a

instance Pure AccordionAccordion ms where
    render AccordionAccordion_ {..} =
        let
            cs =
                ( "accordion"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (AccordionAccordion ms) where
    type AsProp (AccordionAccordion ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a acc = acc { as = a }

instance HasAttributesProp (AccordionAccordion ms) where
    type Attribute (AccordionAccordion ms) = Feature ms
    getAttributes = attributes
    setAttributes as a = a { attributes = as }

instance HasChildrenProp (AccordionAccordion ms) where
    type Child (AccordionAccordion ms) = View ms
    getChildren = children
    setChildren cs a = a { children = cs }

instance HasClassesProp (AccordionAccordion ms) where
    getClasses = classes
    setClasses cs a = a { classes = cs }
