module Semantic.Addons.Accordion.AccordionAccordion where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data AccordionAccordion ms = AccordionAccordion_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (AccordionAccordion ms) where
    def = (G.to gdef) { as = Div }

pattern AccordionAccordion :: Typeable ms => AccordionAccordion ms -> View ms
pattern AccordionAccordion a = View a

instance Typeable ms => Pure AccordionAccordion ms where
    render AccordionAccordion_ {..} =
        let
            cs =
                ( "accordion"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children
