module Semantic.Elements.Step.StepContent where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes

data StepContent ms = StepContent_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (StepContent ms) where
    def = (G.to gdef) { as = Div }

pattern StepContent :: StepContent ms -> View ms
pattern StepContent sc = View sc

instance Pure StepContent ms where
    render StepContent_ {..} =
        let
            cs =
                ( "content"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (StepContent ms) where
    type AsProp (StepContent ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a sc = sc { as = a }

instance HasAttributesProp (StepContent ms) where
    type Attribute (StepContent ms) = Feature ms
    getAttributes = attributes
    setAttributes as sc = sc { attributes = as }

instance HasChildrenProp (StepContent ms) where
    type Child (StepContent ms) = View ms
    getChildren = children
    setChildren cs sc = sc { children = cs }

instance HasClassesProp (StepContent ms) where
    getClasses = classes
    setClasses cs sc = sc { classes = cs }