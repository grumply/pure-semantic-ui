module Semantic.Addons.Accordion.AccordionTitle where

import GHC.Generics as G
import Pure.View hiding (active,onClick)

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Active
import Semantic.Properties.Index
import Semantic.Properties.OnClick

data AccordionTitle ms = AccordionTitle_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    , index :: Int
    , onClick :: Ef ms IO ()
    } deriving (Generic)

instance Default (AccordionTitle ms) where
    def = (G.to gdef) { as = Div }

pattern AccordionTitle :: Typeable ms => AccordionTitle ms -> View ms
pattern AccordionTitle at = View at

instance Typeable ms => Pure AccordionTitle ms where
    render AccordionTitle_ {..} =
        let
            cs =
                ( active # "active"
                : "title"
                : classes
                )
        in
            as
                ( ClassList cs
                : onClick # (On "click" def $ \_ -> return (Just onClick))
                : attributes
                )
                children

instance HasAsProp (AccordionTitle ms) where
    type AsProp (AccordionTitle ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a at = at { as = a }

instance HasAttributesProp (AccordionTitle ms) where
    type Attribute (AccordionTitle ms) = Feature ms
    getAttributes = attributes
    setAttributes as at = at { attributes = as }

instance HasChildrenProp (AccordionTitle ms) where
    type Child (AccordionTitle ms) = View ms
    getChildren = children
    setChildren cs at = at { children = cs }

instance HasClassesProp (AccordionTitle ms) where
    getClasses = classes
    setClasses cs at = at { classes = cs }

instance HasActiveProp (AccordionTitle ms) where
    getActive = active
    setActive a at = at { active = a }

instance HasIndexProp (AccordionTitle ms) where
    getIndex = index
    setIndex i at = at { index = i }

instance HasOnClickProp (AccordionTitle ms) where
    type OnClickProp (AccordionTitle ms) = Ef ms IO ()
    getOnClick = onClick
    setOnClick oc at = at { onClick = oc }