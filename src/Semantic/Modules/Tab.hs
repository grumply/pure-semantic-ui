module Semantic.Modules.Tab where

import GHC.Generics as G
import Pure.View hiding (active)

import Semantic.Utils

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  , HasActiveProp(..), pattern Active
  , HasLoadingProp(..), pattern Loading
  )

import Semantic.Properties
  ( HasAttachedProp(..), pattern Attached )

import Semantic.Elements.Segment (pattern Segment)

data Tab ms = Tab_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Tab ms) where
    def = (G.to gdef) { as = Div }

pattern Tab :: Tab ms -> View ms
pattern Tab t = View t

instance Pure Tab ms where
    render Tab_ {..} = as ( ClassList classes : attributes ) children

instance HasAsProp (Tab ms) where
    type AsProp (Tab ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f t = t { as = f }

instance HasAttributesProp (Tab ms) where
    type Attribute (Tab ms) = Feature ms
    getAttributes = attributes
    setAttributes cs t = t { attributes = cs }

instance HasChildrenProp (Tab ms) where
    type Child (Tab ms) = View ms
    getChildren = children
    setChildren cs t = t { children = cs }

instance HasClassesProp (Tab ms) where
    getClasses = classes
    setClasses cs t = t { classes = cs }

data Pane ms = Pane_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    , loading :: Bool
    } deriving (Generic)

instance Default (Pane ms) where
    def = (G.to gdef)
        { as = \fs cs ->
            Segment $ def
                & Attached "bottom"
                & Attributes fs
                & Children cs
        }

pattern Pane :: Pane ms -> View ms
pattern Pane tp = View tp

instance Pure Pane ms where
    render Pane_ {..} =
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

instance HasAsProp (Pane ms) where
    type AsProp (Pane ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f tp = tp { as = f }

instance HasAttributesProp (Pane ms) where
    type Attribute (Pane ms) = Feature ms
    getAttributes = attributes
    setAttributes cs tp = tp { attributes = cs }

instance HasChildrenProp (Pane ms) where
    type Child (Pane ms) = View ms
    getChildren = children
    setChildren cs tp = tp { children = cs }

instance HasClassesProp (Pane ms) where
    getClasses = classes
    setClasses cs tp = tp { classes = cs }

instance HasActiveProp (Pane ms) where
    getActive = active
    setActive a tp = tp { active = a }

instance HasLoadingProp (Pane ms) where
    getLoading = loading
    setLoading l tp = tp { loading = l }
