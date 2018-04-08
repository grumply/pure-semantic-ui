module Semantic.Modules.Tab (module Semantic.Modules.Tab, module Export) where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  )
  
import Semantic.Modules.Tab.TabPane as Export

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
