module Semantic.Elements.Step.StepDescription where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  )

data StepDescription ms = StepDescription_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (StepDescription ms) where
    def = (G.to gdef) { as = Div }

pattern StepDescription :: StepDescription ms -> View ms
pattern StepDescription sd = View sd

instance Pure StepDescription ms where
    render StepDescription_ {..} =
        let
            cs =
                ( "description"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (StepDescription ms) where
    type AsProp (StepDescription ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a sd = sd { as = a }

instance HasAttributesProp (StepDescription ms) where
    type Attribute (StepDescription ms) = Feature ms
    getAttributes = attributes
    setAttributes as sd = sd { attributes = as }

instance HasChildrenProp (StepDescription ms) where
    type Child (StepDescription ms) = View ms
    getChildren = children
    setChildren cs sd = sd { children = cs }

instance HasClassesProp (StepDescription ms) where
    getClasses = classes
    setClasses cs sd = sd { classes = cs }
