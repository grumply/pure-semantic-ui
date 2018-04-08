module Semantic.Modules.Dropdown.DropdownMenu where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  , HasScrollingProp(..), pattern Scrolling
  )

data DropdownMenu ms = DropdownMenu_ 
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms] 
    , children :: [View ms]
    , classes :: [Txt]
    , scrolling :: Bool
    } deriving (Generic)

instance Default (DropdownMenu ms) where
    def = (G.to gdef) { as = Div }

pattern DropdownMenu :: DropdownMenu ms -> View ms
pattern DropdownMenu dm = View dm

instance Pure DropdownMenu ms where
    render DropdownMenu_ {..} =
        let
            cs = 
                ( scrolling # "scrolling"
                : "menu transition"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (DropdownMenu ms) where
    type AsProp (DropdownMenu ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f dm = dm { as = f }

instance HasAttributesProp (DropdownMenu ms) where
    type Attribute (DropdownMenu ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs dm = dm { attributes = cs }

instance HasChildrenProp (DropdownMenu ms) where
    type Child (DropdownMenu ms) = View ms
    getChildren = children
    setChildren cs dm = dm { children = cs }

instance HasClassesProp (DropdownMenu ms) where
    getClasses = classes
    setClasses cs dm = dm { classes = cs }

instance HasScrollingProp (DropdownMenu ms) where
    getScrolling = scrolling
    setScrolling s dm = dm { scrolling = s }
