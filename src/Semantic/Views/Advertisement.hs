module Semantic.Views.Advertisement where

import GHC.Generics as G
import Pure.View hiding (unit)

import Semantic.Utils

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  , HasCenteredProp(..), pattern Centered
  , HasTestProp(..), pattern Test
  , HasUnitProp(..), pattern Unit
  )

data Advertisement ms = Advertisement_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , centered :: Bool
    , test :: Txt
    , unit :: Txt
    } deriving (Generic)

instance Default (Advertisement ms) where
    def = (G.to gdef) { as = Div }

pattern Advertisement :: Advertisement ms -> View ms
pattern Advertisement a = View a

instance Pure Advertisement ms where
    render Advertisement_ {..} =
        let
            cs =
                ( "ui"
                : unit
                : centered # "centered"
                : test # "test"
                : "ad"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : test # (Property "data-text" test)
                : attributes
                )
                children

instance HasAsProp (Advertisement ms) where
    type AsProp (Advertisement ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a ad = ad { as = a }

instance HasAttributesProp (Advertisement ms) where
    type Attribute (Advertisement ms) = Feature ms
    getAttributes = attributes
    setAttributes as a = a { attributes = as }

instance HasChildrenProp (Advertisement ms) where
    type Child (Advertisement ms) = View ms
    getChildren = children
    setChildren cs a = a { children = cs }

instance HasClassesProp (Advertisement ms) where
    getClasses = classes
    setClasses cs a = a { classes = cs }

instance HasCenteredProp (Advertisement ms) where
    getCentered = centered
    setCentered c a = a { centered = c }

instance HasTestProp (Advertisement ms) where
    getTest = test
    setTest t a = a { test = t }

instance HasUnitProp (Advertisement ms) where
    getUnit = unit
    setUnit u a = a { unit = u }
