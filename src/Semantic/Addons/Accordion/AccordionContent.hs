module Semantic.Addons.Accordion.AccordionContent where

import GHC.Generics as G
import Pure.View hiding (active)

import Semantic.Utils

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
