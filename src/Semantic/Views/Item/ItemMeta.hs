module Semantic.Views.Item.ItemMeta where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  )

data ItemMeta ms = ItemMeta_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (ItemMeta ms) where
    def = (G.to gdef) { as = Div }

pattern ItemMeta :: ItemMeta ms -> View ms
pattern ItemMeta im = View im

instance Pure ItemMeta ms where
    render ItemMeta_ {..} =
        let
            cs =
                ( "meta"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (ItemMeta ms) where
    type AsProp (ItemMeta ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a im = im { as = a }

instance HasAttributesProp (ItemMeta ms) where
    type Attribute (ItemMeta ms) = Feature ms
    getAttributes = attributes
    setAttributes as im = im { attributes = as }

instance HasChildrenProp (ItemMeta ms) where
    type Child (ItemMeta ms) = View ms
    getChildren = children
    setChildren cs im = im { children = cs }

instance HasClassesProp (ItemMeta ms) where
    getClasses = classes
    setClasses cs im = im { classes = cs }
