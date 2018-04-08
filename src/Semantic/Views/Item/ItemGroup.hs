module Semantic.Views.Item.ItemGroup where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  , HasDividedProp(..), pattern Divided
  , HasLinkProp(..), pattern Link
  , HasRelaxedProp(..), pattern Relaxed
  , HasUnstackableProp(..), pattern Unstackable
  )

data ItemGroup ms = ItemGroup_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , divided :: Bool
    , link :: Bool
    , relaxed :: Maybe Txt
    , unstackable :: Bool
    } deriving (Generic)

instance Default (ItemGroup ms) where
    def = (G.to gdef) { as = Div }

pattern ItemGroup :: ItemGroup ms -> View ms
pattern ItemGroup ig = View ig

instance Pure ItemGroup ms where
    render ItemGroup_ {..} =
        let
            cs =
                ( "ui"
                : divided # "divided"
                : link # "link"
                : unstackable # "unstackable"
                : may (<>> "relaxed") relaxed
                : "items"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (ItemGroup ms) where
    type AsProp (ItemGroup ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a ig = ig { as = a }

instance HasAttributesProp (ItemGroup ms) where
    type Attribute (ItemGroup ms) = Feature ms
    getAttributes = attributes
    setAttributes as ig = ig { attributes = as }

instance HasChildrenProp (ItemGroup ms) where
    type Child (ItemGroup ms) = View ms
    getChildren = children
    setChildren cs ig = ig { children = cs }

instance HasClassesProp (ItemGroup ms) where
    getClasses = classes
    setClasses cs ig = ig { classes = cs }

instance HasDividedProp (ItemGroup ms) where
    getDivided = divided
    setDivided d ig = ig { divided = d }

instance HasLinkProp (ItemGroup ms) where
    getLink = link
    setLink l ig = ig { link = l }

instance HasRelaxedProp (ItemGroup ms) where
    getRelaxed = relaxed
    setRelaxed r ig = ig { relaxed = r }

instance HasUnstackableProp (ItemGroup ms) where
    getUnstackable = unstackable
    setUnstackable u ig = ig { unstackable = u }
