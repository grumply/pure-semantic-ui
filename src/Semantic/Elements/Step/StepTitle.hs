module Semantic.Elements.Step.StepTitle where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes

data StepTitle ms = StepTitle_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (StepTitle ms) where
    def = (G.to gdef) { as = Div }

pattern StepTitle :: Typeable ms => StepTitle ms -> View ms
pattern StepTitle st = View st

instance Typeable ms => Pure StepTitle ms where
    render StepTitle_ {..} =
        let
            cs =
                ( "title"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (StepTitle ms) where
    type AsProp (StepTitle ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a st = st { as = a }

instance HasAttributesProp (StepTitle ms) where
    type Attribute (StepTitle ms) = Feature ms
    getAttributes = attributes
    setAttributes as st = st { attributes = as }

instance HasChildrenProp (StepTitle ms) where
    type Child (StepTitle ms) = View ms
    getChildren = children
    setChildren cs st = st { children = cs }

instance HasClassesProp (StepTitle ms) where
    getClasses = classes
    setClasses cs st = st { classes = cs }

