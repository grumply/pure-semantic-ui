module Semantic.Views.Item (module Semantic.Views.Item, module Export) where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  )

import Semantic.Views.Item.ItemContent as Export
import Semantic.Views.Item.ItemDescription as Export
import Semantic.Views.Item.ItemExtra as Export
import Semantic.Views.Item.ItemGroup as Export
import Semantic.Views.Item.ItemHeader as Export
import Semantic.Views.Item.ItemImage as Export
import Semantic.Views.Item.ItemMeta as Export

data Item ms = Item_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Item ms) where
    def = (G.to gdef) { as = Div }

pattern Item :: Item ms -> View ms
pattern Item i = View i

instance Pure Item ms where
    render Item_ {..} =
        let
            cs =
                ( "item"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (Item ms) where
    type AsProp (Item ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a i = i { as = a }

instance HasAttributesProp (Item ms) where
    type Attribute (Item ms) = Feature ms
    getAttributes = attributes
    setAttributes as i = i { attributes = as }

instance HasChildrenProp (Item ms) where
    type Child (Item ms) = View ms
    getChildren = children
    setChildren cs i = i { children = cs }

instance HasClassesProp (Item ms) where
    getClasses = classes
    setClasses cs i = i { classes = cs }
