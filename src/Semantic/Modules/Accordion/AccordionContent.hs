module Semantic.Modules.Accordion.AccordionContent where

import GHC.Generics as G
import Pure.View hiding (active)

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Active

data AccordionContent ms = AccordionContent_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    } deriving (Generic)

instance Default (AccordionContent ms) where
    def = (G.to gdef) { as = Div }

pattern AccordionContent :: Typeable ms => AccordionContent ms -> View ms
pattern AccordionContent ac = View ac

instance Typeable ms => Pure AccordionContent ms where
    render AccordionContent_ {..} =
        let
            cs =
                ( "content"
                : active # "active"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children

instance HasAsProp (AccordionContent ms) where
    type AsProp (AccordionContent ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a ac = ac { as = a }

instance HasAttributesProp (AccordionContent ms) where
    type Attribute (AccordionContent ms) = Feature ms
    getAttributes = attributes
    setAttributes as ac = ac { attributes = as }

instance HasChildrenProp (AccordionContent ms) where
    type Child (AccordionContent ms) = View ms
    getChildren = children
    setChildren cs ac = ac { children = cs }

instance HasClassesProp (AccordionContent ms) where
    getClasses = classes
    setClasses cs ac = ac { classes = cs }

instance HasActiveProp (AccordionContent ms) where
    getActive = active
    setActive a ac = ac { active = a }