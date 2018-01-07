module Semantic.Modules.Tab.TabPane where

import GHC.Generics as G
import Pure.View hiding (active)

import Semantic.Utils

import Semantic.Elements.Segment

import Semantic.Properties.Attached

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Active
import Semantic.Properties.Loading

data TabPane ms = TabPane_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    , loading :: Bool
    } deriving (Generic)

instance  Default (TabPane ms) where
    def = (G.to gdef) 
        { as = \fs cs -> 
            Segment $ def 
                & Attached "bottom" 
                & Attributes fs 
                & Children cs 
        }

pattern TabPane :: TabPane ms -> View ms
pattern TabPane tp = View tp

instance Pure TabPane ms where
    render TabPane_ {..} =
        let
            cs =
                ( active # "active"
                : loading # "loading"
                : "tab"
                : classes
                )
        in
            as 
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (TabPane ms) where
    type AsProp (TabPane ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f tp = tp { as = f }

instance HasAttributesProp (TabPane ms) where
    type Attribute (TabPane ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs tp = tp { attributes = cs }

instance HasChildrenProp (TabPane ms) where
    type Child (TabPane ms) = View ms
    getChildren = children
    setChildren cs tp = tp { children = cs }

instance HasClassesProp (TabPane ms) where
    getClasses = classes
    setClasses cs tp = tp { classes = cs }

instance HasActiveProp (TabPane ms) where
    getActive = active
    setActive a tp = tp { active = a }

instance HasLoadingProp (TabPane ms) where
    getLoading = loading
    setLoading l tp = tp { loading = l }